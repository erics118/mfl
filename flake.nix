{
  description = "OCaml development shell";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    { nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };
        # ocaml llvm does not compile properly on macos
        ocamlLlvm =
          if pkgs.stdenv.isDarwin then
            pkgs.ocamlPackages.llvm.overrideAttrs (old: {
              postPatch = (old.postPatch or "") + ''
                substituteInPlace llvm/cmake/modules/AddOCaml.cmake \
                  --replace-fail '"''${bin}/dll''${name}''${CMAKE_SHARED_LIBRARY_SUFFIX}"' '"''${bin}/dll''${name}.so"'
                substituteInPlace llvm/cmake/modules/AddOCaml.cmake \
                  --replace-fail 'ext STREQUAL CMAKE_SHARED_LIBRARY_SUFFIX' 'ext STREQUAL ".so"'
              '';
            })
          else
            pkgs.ocamlPackages.llvm;
      in
      {
        devShells.default = pkgs.mkShell {
          packages = with pkgs; [
            ocaml
            opam
            dune_3
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
      }
    );
}
