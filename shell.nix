{ pkgs ? import <nixpkgs> {}
}:

pkgs.haskellPackages.shellFor {
  packages = _: [ (import ./default.nix {}).pkg ];
  withHoogle = true;
}
