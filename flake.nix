{
  description = "OCaml development shell";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
  };

  outputs =
    inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];

      perSystem =
        { pkgs, ... }:
        let
          # ocaml llvm bindings do not compile properly on macos without this fix
          ocamlLlvm =
            if pkgs.stdenv.isDarwin then
              pkgs.ocamlPackages.llvm.overrideAttrs (old: {
                postPatch = (old.postPatch or "") + ''
                  substituteInPlace llvm/cmake/modules/AddOCaml.cmake \
                    --replace-fail '"''${bin}/dll''${name}''${CMAKE_SHARED_LIBRARY_SUFFIX}"' '"''${bin}/dll''${name}.so"'
                  substituteInPlace llvm/cmake/modules/AddOCaml.cmake \
                    --replace-fail 'ext STREQUAL CMAKE_SHARED_LIBRARY_SUFFIX' 'ext STREQUAL ".so"'
                  sed -i '/-Wl,-rpath.*CAMLORIGIN/d' llvm/cmake/modules/AddOCaml.cmake
                '';
              })
            else
              pkgs.ocamlPackages.llvm;
        in
        {
          devShells.default = pkgs.mkShell {
            NIX_CFLAGS_COMPILE = "-Wno-override-module";
            OCAMLRUNPARAM = "b,v=61";
            packages = with pkgs; [
              clang
              ocaml
              opam
              dune_3
              ocamlPackages.odoc
              ocamlPackages.ocaml-lsp
              ocamlPackages.ocamlformat
              ocamlPackages.merlin
              ocamlPackages.utop
              ocamlPackages.ounit2
              ocamlPackages.ppxlib
              ocamlPackages.bisect_ppx
              ocamlLlvm
            ];
          };
        };
    };
}
