# Dotfiles

My dotfiles repo.

## Home Manager with Nix Flakes

### Installing Home Manager, Linux Standalone Installation

``` sh
bash ./scripts/bootstrap-x86_64-linux.sh
```

or using this repository's flake URI, i.e. `github:rgoulter/dotfiles`

```
nix build --no-link github:rgoulter/dotfiles#homeConfigurations.rgoulter-x86_64-linux.activationPackage
"$(nix path-info github:rgoulter/dotfiles#homeConfigurations.rgoulter-x86_64-linux.activationPackage)"/activate
```

### Using `home.nix` Configuration

From this repository's directory:

```
home-manager switch --flake '.#rgoulter-x86_64-linux'
```

or using this repository's flake URI, i.e. `github:rgoulter/dotfiles`

```
home-manager switch --flake 'github:rgoulter/dotfiles#rgoulter-x86_64-linux'
```

### Using these dotfiles in Your Home Manager Configuration

I found it useful to know that you can use Home Manager modules
from different repositories.

Here's an example of configuring Home Manager in a `flake.nix` file,
using the `home.nix` file (exported as `nixosModules.dotfiles`) in
your `flake.nix`:

``` nix
{
  description = "Home Manager configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    rgoulter = {
      url = "github:rgoulter/dotfiles";
      inputs = {
        home-manager.follows = "home-manager";
        nixpkgs.follows = "nixpkgs";
      };
    }
  };

  outputs = { nixpkgs, home-manager, ... }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
    in {
      homeConfigurations.jdoe = home-manager.lib.homeManagerConfiguration {
        inherit pkgs;

        modules = [
          rgoulter.nixosModules.dotfiles
          # ... other modules
        ];
      };
    };
}
```
