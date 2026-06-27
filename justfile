# e.g. x86_64-linux or x86_64-darwin or x86_64-macos.
system := arch() + "-" + os()
# e.g. rgoulter or richardgoulter
user := env_var("USER")
# e.g. "." or "github:rgoulter/dotfiles"
flake_uri := "."

default: build

# Validate the Home Manager configuration without applying it.
build:
  nix run {{flake_uri}}#home-manager -- build --flake {{flake_uri}}#{{user}}-{{system}}

# Apply the Home Manager configuration (symlinks, managed files, etc.).
switch:
  nix run {{flake_uri}}#home-manager -- switch --flake {{flake_uri}}#{{user}}-{{system}}

alias apply := switch

fmt:
  nix run {{flake_uri}} --
