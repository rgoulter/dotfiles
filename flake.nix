{
  description = "rgoulter's Home Manager configuration";

  inputs = {
    devenv.url = "github:cachix/devenv";
    flake-parts.url = "github:hercules-ci/flake-parts";
    home-manager.url = "github:nix-community/home-manager";
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    systems.url = "github:nix-systems/default";
    treefmt-nix.url = "github:numtide/treefmt-nix";
  };

  outputs = inputs @ {
    self,
    devenv,
    flake-parts,
    home-manager,
    nixpkgs,
    systems,
    treefmt-nix,
    ...
  }:
    flake-parts.lib.mkFlake {inherit inputs;} {
      systems = import systems;

      imports = [
        treefmt-nix.flakeModule
      ];

      flake = {
        homeConfigurations = {
          "richardgoulter-x86_64-darwin" = let
            system = "x86_64-darwin";
            pkgs = nixpkgs.legacyPackages.${system};
          in
            home-manager.lib.homeManagerConfiguration {
              inherit pkgs;

              modules = [
                self.nixosModules.dotfiles
                {
                  home = {
                    username = "richardgoulter";
                    homeDirectory = "/Users/richardgoulter";
                    stateVersion = "22.05";
                  };
                  programs.home-manager.enable = true;
                }
              ];
            };

          "rgoulter-x86_64-linux" = let
            system = "x86_64-linux";
            pkgs = nixpkgs.legacyPackages.${system};
          in
            home-manager.lib.homeManagerConfiguration {
              inherit pkgs;

              modules = [
                self.nixosModules.dotfiles
                {
                  home = {
                    username = "rgoulter";
                    homeDirectory = "/home/rgoulter";
                    stateVersion = "22.05";
                  };
                  programs.home-manager.enable = true;
                }
              ];
            };

          "richardgoulter-x86_64-macos" = self.homeConfigurations.richardgoulter-x86_64-darwin;
        };

        lib = {
          configSymlinks = import ./lib/configSymlinks.nix;
        };

        nixosModules = {
          default = self.nixosModules.dotfiles;
          dotfiles = import ./dotfiles.nix;
        };
      };

      perSystem = {
        config,
        pkgs,
        system,
        ...
      }: {
        devShells = {
          default = devenv.lib.mkShell {
            inherit inputs pkgs;

            modules = [
              ({pkgs, ...}: {
                packages = with pkgs; [
                  # add treefmt using flake-parts per-system config
                  config.treefmt.build.wrapper
                ];

                languages = {
                  nix.enable = true;
                  shell.enable = true;
                };
              })
            ];
          };
        };

        treefmt = import ./treefmt.nix;
      };
    };
}
