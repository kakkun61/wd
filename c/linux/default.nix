let pkgs = import <nixpkgs> {};
in
pkgs.stdenv.mkDerivation {
  pname = "wd";
  version = "0.1.0";
  src = ./.;
}
