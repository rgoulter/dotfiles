{
  description = "rgoulter's Home Manager configuration";

  inputs = {
    devenv.url = "github:cachix/devenv";
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    home-manager.url = "github:nix-community/home-manager";
    systems.url = "github:nix-systems/default";
    treefmt-nix.url = "github:numtide/treefmt-nix";
  };

  outputs = inputs @ {
    self,
    devenv,
    home-manager,
    nixpkgs,
    systems,
    treefmt-nix,
    ...
  }: let
    forAllSystems = f: nixpkgs.lib.genAttrs (import systems) (system: f system);
    treefmtEval = forAllSystems (system: treefmt-nix.lib.evalModule nixpkgs.legacyPackages.${system} ./treefmt.nix);
  in {
    checks = forAllSystems (system: {
      formatting = treefmtEval.${system}.config.build.check self;
    });
    devShells = forAllSystems (system: let
      pkgs = nixpkgs.legacyPackages.${system};
    in {
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
    });
    formatter = forAllSystems (system: treefmtEval.${system}.config.build.wrapper);
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
    nixosModules = {
      default = self.nixosModules.dotfiles;
      dotfiles = import ./dotfiles.nix;
    };
    lib = {
      configSymlinks = import ./lib/configSymlinks.nix;
    };
  };
}
