{ pkgs ? import <nixpkgs> { }
}:
let
  easy-ps = import (builtins.fetchGit {
    url = "https://github.com:justinwoo/easy-purescript-nix.git";
    rev = "1ec689df0adf8e8ada7fcfcb513876307ea34226";
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

