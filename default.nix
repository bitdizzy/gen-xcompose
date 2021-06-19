{ pkgs ? (import <nixpkgs> {})
}:

rec {
  pkg = pkgs.haskellPackages.callPackage ./package.nix {};
  output = import ./output.nix { inherit (pkgs) runCommand; gen-xcompose = pkg; };
}
