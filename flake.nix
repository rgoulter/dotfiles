{
  description = "rgoulter's Home Manager configuration";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    home-manager = {
      # https://github.com/nix-community/home-manager/pull/2461
      inputs.nixpkgs.follows = "nixpkgs";
      # Fork which allows using `nix profile` instead of `nix-env`
      # https://github.com/FlorianFranzen/home-manager/commit/4e97b01b2737bb0f39c18a65d87dd98659391b97
      # url = {
      type  = "github";
      owner = "FlorianFranzen";
      repo  = "home-manager";
      rev   = "4e97b01b2737bb0f39c18a65d87dd98659391b97";
    };
  };

  outputs = { home-manager, ... }: {
      homeConfigurations = {
        "richardgoulter-x86_64-darwin" = home-manager.lib.homeManagerConfiguration {
          configuration = import ./home.nix;
          homeDirectory = "/Users/richardgoulter";
          stateVersion = "22.05";
          system = "x86_64-darwin";
          username = "richard";
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
