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
          ];

  shellHook = ''
            echo "python39 = ${python39}"
            echo "libX11 = ${xorg.libX11}"
  '';
}
