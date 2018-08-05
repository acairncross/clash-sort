{ pkgs ? import <nixpkgs> {} }:

let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          clash-prelude =
            pkgs.haskell.lib.dontCheck
              (haskellPackagesNew.callCabal2nix
                "clash-prelude"
                (pkgs.fetchFromGitHub {
                  owner = "clash-lang";
                  repo = "clash-prelude";
                  rev = "8a0241758e84e308ec9425229e1ce30152870423";
                  sha256 = "0c8bxic0c3k3h2yxkl88fhpihay0gpvmzzb6g3z3i7y7nnkxvh8g";
                })
                {});

          clashSrc = pkgs.srcOnly {
            name = "clash-source";
            src = pkgs.fetchFromGitHub {
               owner = "clash-lang";
               repo = "clash-compiler";
               rev = "f07589743267650c9385216863ae9eb3b255d83d";
               sha256 = "0kdd5n818mx5nh64m4c3bg16grn80fi38lss5ih85b7v0rxqaprz";
            };
          };

          clash-ghc =
            pkgs.haskell.lib.dontCheck
            (haskellPackagesNew.callCabal2nix "clash-ghc" "${clashSrc}/clash-ghc" {});

          clash-lib =
            pkgs.haskell.lib.dontCheck
            (haskellPackagesNew.callCabal2nix "clash-lib" "${clashSrc}/clash-lib" {});
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

in
  { pkgs = pkgs;
    clash-sort = pkgs.haskellPackages.callCabal2nix "clash-sort" ./. {};
  }
