{ nixpkgs ? import <nixpkgs> {}
}:
let
  inherit (nixpkgs) pkgs;

  drv = pkgs.haskellPackages.callCabal2nix "cpals" ./. {};
in
  pkgs.haskell.lib.shellAware drv
