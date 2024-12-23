{
  description = "This is a command to run another command on a specified directory.";

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixos-23.05;
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
      in
      {
        packages.default =
          with pkgs; stdenv.mkDerivation {
            pname = "wd";
            version = "0.1.0";
            src = ./.;
          };
        devShells.default = pkgs.mkShell {
          packages = with pkgs; [
            clang-tools
            nixpkgs-fmt
            stdenv
          ];
        };
      }
    );
}
