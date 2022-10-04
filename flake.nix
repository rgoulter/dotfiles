{
  description = "rgoulter's Home Manager configuration";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    home-manager.url = "github:nix-community/home-manager";
  };

  outputs = { self, home-manager, nixpkgs, ... }: {
      nixosModules = {
        default = self.nixosModules.dotfiles;
        dotfiles = import ./dotfiles.nix;
      };
      homeConfigurations = {
        "richardgoulter-x86_64-darwin" =
          let
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
              }
            ];
          };

        "rgoulter-x86_64-linux" =
          let
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
              }
            ];
          };
      };
    };
}
