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

  outputs = { home-manager, ... }:
    let
      system = "x86_64-linux";
      username = "rgoulter";
    in {
      homeConfigurations."${username}-${system}" = home-manager.lib.homeManagerConfiguration {
        inherit system username;

        configuration = import ./home.nix;

        homeDirectory = "/home/${username}";

        # Update the state version as needed.
        # See the changelog here:
        # https://nix-community.github.io/home-manager/release-notes.html
        stateVersion = "22.05";
      };
    };
}
