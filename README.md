# Dotfiles

My dotfiles repo.

Makes use of [anishathalye/dotbot](https://github.com/anishathalye/dotbot)
for managing symlinks.

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

## Vim Native Dependencies

A couple of plugins require native dependencies. (VimProc, and YouCompleteMe).

For building YCM with C/C++ support, with system libclang (not recommended by YCM):

```
cmake -G "Unix Makefiles" -DUSE_SYSTEM_LIBCLANG=ON . ~/.vim/bundle/YouCompleteMe/third_party/ycmd/cpp
```
