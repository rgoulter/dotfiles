# e.g. x86_64-linux or x86_64-darwin or x86_64-macos.
system := arch() + "-" + os()
# e.g. rgoulter or richardgoulter
user := env_var("USER")
# e.g. "." or "github:rgoulter/dotfiles"
flake_uri := "."

default: home-manager-switch

bootstrap:
  ./scripts/bootstrap-{{system}}.sh

home-manager-switch:
  home-manager switch --flake {{flake_uri}}#{{user}}-{{system}}
