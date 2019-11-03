{ nixpkgs ? import ./nixpkgs.nix {} }:

nixpkgs.haskellPackages.callCabal2nix "clash-sort" ./. {}
