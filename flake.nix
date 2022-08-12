{
  description = "rgoulter's Home Manager configuration";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    home-manager.url = "github:nix-community/home-manager";
  };

  outputs = { self, home-manager, ... }: {
      nixosModules = {
        default = self.nixosModules.dotfiles;
        dotfiles = import ./home.nix;
      };
      homeConfigurations = {
        "richardgoulter-x86_64-darwin" = home-manager.lib.homeManagerConfiguration {
          configuration = import ./home.nix;
          homeDirectory = "/Users/richardgoulter";
          stateVersion = "22.05";
          system = "x86_64-darwin";
          username = "richardgoulter";
        };

        "rgoulter-x86_64-linux" = home-manager.lib.homeManagerConfiguration {
          configuration = import ./home.nix;
          homeDirectory = "/home/rgoulter";
          stateVersion = "22.05";
          system = "x86_64-linux";
          username = "rgoulter";
        };
      };
    };
}
