# { pkgs ? import <nixpkgs> { }
{ pkgs ? import (builtins.fetchGit {
  # https://github.com/NixOS/nixpkgs/releases/tag/20.09
  url = "https://github.com/nixos/nixpkgs/";
  ref = "refs/tags/20.09";
  rev = "cd63096d6d887d689543a0b97743d28995bc9bc3";
  }) {}
}:
let
  easy-ps = import (builtins.fetchGit {
    url = "https://github.com/justinwoo/easy-purescript-nix.git";
    rev = "47bdc016c7d56e987ca1aca690b1d6c9816a8584";
  }) { inherit pkgs; };
in
pkgs.mkShell {
  buildInputs = [
    easy-ps.purs-0_13_8
    easy-ps.spago
    easy-ps.pulp
    pkgs.nodejs-14_x
    pkgs.nodePackages.bower
  ];
  LC_ALL = "C.UTF-8"; # https://github.com/purescript/spago/issues/507
}

