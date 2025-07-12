{
  description = "This is a command to run another command on a specified directory.";

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixos-23.05;
    flake-parts.url = "github:hercules-ci/flake-parts";
  };

  outputs = inputs@{ self, nixpkgs, flake-parts }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      perSystem = { pkgs, ... }: {
        packages.default =
          with pkgs; stdenv.mkDerivation rec {
            pname = "wd";
            version = "1.2.0";
            src = ./.;
            meta = with lib; {
              homepage = https://github.com/kakkun61/wd;
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
            nixpkgs-fmt
            stdenv
          ];
        };
      };
    };
}
