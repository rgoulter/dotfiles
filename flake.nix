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
  }: let
    forAllSystems = f: nixpkgs.lib.genAttrs (import systems) (system: f system);
    treefmtEval = forAllSystems (system: treefmt-nix.lib.evalModule nixpkgs.legacyPackages.${system} ./treefmt.nix);
  in
    flake-parts.lib.mkFlake {inherit inputs;} {
      systems = import systems;

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

      perSystem = {config, pkgs, system, ...}: {
        checks = {
          formatting = treefmtEval.${system}.config.build.check self;
        };

        devShells = {
          default = devenv.lib.mkShell {
            inherit inputs pkgs;

            modules = [
              ({
                pkgs,
                  config,
                  ...
              }: {
                packages = with pkgs; [
                  treefmt
                ];

                languages = {
                  nix.enable = true;
                  shell.enable = true;
                };
              })
            ];
          };
        };

        formatter = treefmtEval.${system}.config.build.wrapper;
      };
    };
}
