{
  description = "This is a command to run another command on a specified directory.";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-parts.url = "github:hercules-ci/flake-parts";
  };

  outputs =
    inputs@{
      self,
      nixpkgs,
      treefmt-nix,
      flake-parts,
    }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [ treefmt-nix.flakeModule ];
      systems = nixpkgs.lib.systems.flakeExposed;
      perSystem =
        { pkgs, ... }:
        {
          packages.default =
            with pkgs;
            stdenv.mkDerivation rec {
              pname = "wd";
              version = "1.2.0";
              src = ./linux;
              meta = with lib; {
                homepage = "https://github.com/kakkun61/wd";
                changelog = "https://github.com/kakkun61/wd/releases/tag/${version}";
                license = licenses.gpl3;
                maintainers = [
                  { name = "Kazuki Okamoto (岡本和樹)"; }
                ];
                platforms = platforms.linux ++ platforms.darwin;
              };
            };
          devShells.default = pkgs.mkShell {
            packages = with pkgs; [
              clang-tools
              stdenv
            ];
          };
          treefmt = {
            programs = {
              clang-format.enable = true;
              nixfmt.enable = true;
            };
            settings.formatter.clang-format.args = [ "--style=file" ];
          };
        };
    };
}
