{ pkgs ? import ./nix/nixpkgs { } }:

let
  #lab-script = pkgs.writeShellScriptBin "lab" (builtins.readFile ./bin/lab);
in
with pkgs;

mkShell {
  
  packages = [ 
       gcc 
       gnumake bazel_4  
       bison
       flex 
  ];
          
  inputsFrom = [ 
             python39
             xorg.libX11
             xorg.xorgproto
             postgresql
             openssl_1_1
             zlib
          ];

  #nix run github:bennofs/nix-index#nix-locate lib/libssl.so.1.1

  shellHook = ''
            echo "python39 = ${python39}"
            echo "libX11 = ${xorg.libX11}"
            echo "xorg.xorproto = ${xorg.xorgproto}"
            echo "libX11.dev = ${xorg.libX11.dev}"
            echo "postgresql = ${postgresql}"
            echo "postgresql.lib = ${postgresql.lib}"
            echo "openssl_1_1.out = ${openssl_1_1.out}"
            echo "zlib.out/lib = ${zlib.out}/lib"
            export LD_LIBRARY_PATH="${openssl_1_1.out}/lib:${postgresql.lib}/lib:${xorg.libX11}/lib:${zlib.out}/lib"
  '';
}
