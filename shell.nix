{ nixpkgs ? import ./nixpkgs.nix {} }:

(import ./. {}).env.overrideAttrs (old: {
  buildInputs = old.buildInputs ++ [nixpkgs.haskellPackages.clash-ghc];
})
