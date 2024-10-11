{
  description = "C++ environment using Nix flakes";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };

      in
      {
        devShells.default = with pkgs; mkShell {
          name = "flake-example-shell";
          
          packages = [ 
            gcc 
            gnumake 
            bazel_6
            bison  ];
          
          inputsFrom = [ 
             python311
             xorg.libX11.dev
             xorg.xorgproto
             openssl.dev
             zlib
          ];
   
          shellHook = ''
            echo "python311", ${python311}
            echo "openssl.dev", ${openssl.dev}
            echo "libX11/lib", ${xorg.libX11}/lib
            echo "libX11.dev", ${xorg.libX11.dev}
            echo "xorgproto", ${xorg.xorgproto}
            echo "zlib", ${zlib}

            echo Kreiram WORKSPACE.nix ...

            cat <<EOF > WORKSPACE.nix
            workspace(name = "ziher")

            load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

            # BEGIN: Nix dependencies
            # load the http_archive rule itself
            load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

            # load rules_nixpkgs
            http_archive(
                name = "io_tweag_rules_nixpkgs",
                strip_prefix = "rules_nixpkgs-0.9.0",
                urls = ["https://github.com/tweag/rules_nixpkgs/archive/refs/tags/v0.9.0.tar.gz"],
                sha256 = "b01f170580f646ee3cde1ea4c117d00e561afaf3c59eda604cf09194a824ff10",
            )

            # load everything that rules_nixpkgs rules need to work
            load("@io_tweag_rules_nixpkgs//nixpkgs:repositories.bzl", "rules_nixpkgs_dependencies")

            rules_nixpkgs_dependencies()

            load("@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl", "nixpkgs_local_repository", "nixpkgs_cc_configure")
            nixpkgs_local_repository(
                name = "nixpkgs",
                nix_file = "//:nixpkgs.nix",
                nix_file_deps = ["//:flake.lock"],
            )

            http_archive(
                name = "bazel_skylib",
                urls = [
                    "https://mirror.bazel.build/github.com/bazelbuild/bazel-skylib/releases/download/1.0.2/bazel-skylib-1.0.2.tar.gz",
                    "https://github.com/bazelbuild/bazel-skylib/releases/download/1.0.2/bazel-skylib-1.0.2.tar.gz",
                ],
                sha256 = "97e70364e9249702246c0e9444bccdc4b847bed1eb03c5a3ece4f83dfe6abc44",
            )

            http_archive(
                name = "rules_m4",
                urls = ["https://github.com/jmillikin/rules_m4/releases/download/v0.2/rules_m4-v0.2.tar.xz"],
                sha256 = "c67fa9891bb19e9e6c1050003ba648d35383b8cb3c9572f397ad24040fb7f0eb",
            )
            load("@rules_m4//m4:m4.bzl", "m4_register_toolchains")
            m4_register_toolchains()

            new_local_repository(
                name = "postgresql_linux",
                path = "${postgresql_14}/include",
                build_file  = "postgresql_linux.BUILD"
            )

            new_local_repository(
                name = "postgresql_linux_lib",
                path = "${postgresql_14}/lib",
                build_file = "postgresql_linux_lib.BUILD"
            )

            new_local_repository(
                name = "python_linux",
                path="${python311}/include/python3.11",
                build_file = "python_linux.BUILD"
            )

            new_local_repository(
                name = "x11_linux",
                path="${xorg.libX11.dev}/include",
                build_file="x11_linux.BUILD"    
            )

            new_local_repository(
                name = "x11_xorgproto_linux",
                path="${xorg.xorgproto}/include",
                build_file="x11_xorgproto_linux.BUILD"    
            )

            new_local_repository(
                name = "python_linux_lib",
                path="${python311}/lib",
                build_file="python_linux_lib.BUILD"    
            )

            new_local_repository(
                name = "x11_linux_lib",
                path="${xorg.libX11}/lib",
                build_file="x11_linux_lib.BUILD"    
            )

            new_local_repository(
                name = "zlib_linux_lib",
                path="${zlib}/lib",
                build_file="zlib_linux_lib.BUILD"    
            )

            new_local_repository(
                name = "openssl_linux_lib",
                path="${openssl.dev}/lib",
                build_file="openssl_linux_lib.BUILD"    
            )

            # ------ windows ------------------------------------------------------
            new_local_repository(
                name = "python_windows",
                #path = "c:/dev/F18_mono/F18_v3/conan/deploy/x86_64/libpq/include",
                #path = "c:/PostgreSQL/13/include",
                path="C:\\\\Python312\\\\include",
                build_file = "python_windows.BUILD"
            )

            new_local_repository(
                name = "python_windows_lib",
                #path = "c:/dev/F18_mono/F18_v3/conan/deploy/x86_64/libpq/lib",
                #path = "c:/PostgreSQL/13/lib",
                path="C:\\\\Python312\\\\libs",
                build_file="python_windows_lib.BUILD"    
            )

            new_local_repository(
                name = "postgresql_windows",
                #path = "c:/dev/F18_mono/F18_v3/conan/deploy/x86_64/libpq/include",
                #path = "c:/PostgreSQL/13/include",
                path="C:\\\\Program Files\\\\PostgreSQL\\\\14\\\\include",
                build_file = "postgresql_windows.BUILD"
            )

            new_local_repository(
                name = "postgresql_windows_lib",
                #path = "c:/dev/F18_mono/F18_v3/conan/deploy/x86_64/libpq/lib",
                #path = "c:/PostgreSQL/13/lib",
                path="C:\\\\Program Files\\\\PostgreSQL\\\\14\\\\lib",
                build_file="postgresql_windows_lib.BUILD"
                
            )

            new_local_repository(
                name = "postgresql_x86_windows",
                path = "c:/dev/PostgreSQL/x86/10/include",
                build_file="postgresql_windows.BUILD"
            )

            new_local_repository(
                name = "postgresql_x86_windows_lib",
                path = "c:/dev/PostgreSQL/x86/10/lib",
                build_file = "postgresql_windows_lib.BUILD"
            )

            load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

            RULES_JVM_EXTERNAL_TAG = "2.8"
            RULES_JVM_EXTERNAL_SHA = "79c9850690d7614ecdb72d68394f994fef7534b292c4867ce5e7dec0aa7bdfad"

            http_archive(
                name = "rules_jvm_external",
                strip_prefix = "rules_jvm_external-%s" % RULES_JVM_EXTERNAL_TAG,
                sha256 = RULES_JVM_EXTERNAL_SHA,
                url = "https://github.com/bazelbuild/rules_jvm_external/archive/%s.zip" % RULES_JVM_EXTERNAL_TAG,
            )

            load("@rules_jvm_external//:defs.bzl", "maven_install")

            maven_install(
                name = "maven",
                artifacts = [
                    #"com.google.code.findbugs:jsr305:1.3.9",
                    #"com.google.errorprone:error_prone_annotations:2.0.18",
                    #"com.google.j2objc:j2objc-annotations:1.1",
                    "junit:junit:4.12",
                    "org.apache.commons:commons-lang3:3.11",
                    "org.freemarker:freemarker:2.3.31",
                    "org.slf4j:slf4j-api:1.6.1",
                    "commons-io:commons-io:2.9.0",
                    "org.slf4j:slf4j-jdk14:1.6.1",
                    "xml-apis:xml-apis:1.4.01",
                    "org.bluestemsoftware.open.maven.tparty:xerces-impl:2.9.0",
                    "xalan:xalan:2.5.1",
                    "jaxen:jaxen:1.1.6",
                ],
                repositories = [
                    "https://repo1.maven.org/maven2",
                ],
            )

            EOF

            echo "ln -sf WORKSPACE.nix WORKSPACE"
            ln -sf WORKSPACE.nix WORKSPACE


          '';
        };
      });
}
