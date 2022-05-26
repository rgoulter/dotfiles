# Dotfiles

My dotfiles repo.

## Home Manager with Nix Flakes

### Installing Home Manager, Linux Standalone Installation

Per [home-manager manual](https://nix-community.github.io/home-manager/index.html#ch-nix-flakes), in this repository's directory:

```
nix build --no-link .#homeConfigurations.rgoulter-x86_64-linux.activationPackage
"$(nix path-info .#homeConfigurations.rgoulter-x86_64-linux.activationPackage)"/activate
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

