# { pkgs ? (import ./. {}).pkgs {} }:
let
  pkgs = (import ./. {}).pkgs;
in
  ((import ./. {}).clash-sort.env.overrideAttrs (old: {buildInputs = old.buildInputs ++ [pkgs.haskellPackages.clash-ghc];}))
