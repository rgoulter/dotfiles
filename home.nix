{ config, pkgs, ... }:

let
  # Using the submodule in this dotfiles repo would make
  # require a more awkward flake URI.
  vundleRepoSrc = pkgs.fetchFromGitHub {
    owner = "VundleVim";
    repo = "Vundle.vim";
    rev = "cfd3b2d388a8c2e9903d7a9d80a65539aabfe933";
    sha256 = "sha256-OCCXgMVWj/aBWLGaZmMr+cD546+QgynmEN/ECp1r08Q=";
  };

  # Attribute set for dotfiles in this repo to link into ~/.config.
  # The attribute name is for ~/.config/$attrSetName,
  #  e.g. "alacritty/alacritty.yml" for ~/.config/alacritty/alacritty.yml
  # The attribute value is the path to the dotfile in this repo.
  configFilesToLink = {
    "alacritty/alacritty.yml" = ./alacritty/alacritty.yml;
    "emacs-rgoulter/init.el"  = ./emacs.el;
    "emacs-rgoulter/straight/versions/default.el"  = ./emacs.d/straight/versions/default.el;
    "fish/coloured-manpages.fish"  = ./fish/coloured-manpages.fish;
    "fish/config.fish"  = ./fish/config.fish;
    "fish/fishfile"     = ./fish/fishfile;
    "fish/keybindings.txt"        = ./fish/keybindings.txt;
    "fish/functions/fisher.fish"  = ./fish/functions/fisher.fish;
    "fish/functions/fish_greeting.fish"  = ./fish/functions/fish_greeting.fish;
    "git/common.inc"   = ./git/common.inc;
    "kitty/kitty.conf" = ./kitty/kitty.conf;
    "powerline/themes/tmux/default.json" = ./powerline/themes/tmux/default.json;
    "nvim/init.vim" = ./vimrc;
    "starship.toml" = ./starship.toml;
  };

  # Attribute set for dotfiles in this repo to link into home directory.
  # The attribute name is for ~/$attrSetName,
  #  e.g. ".hgrc" for ~/.hgrc.
  # The attribute value is the path to the dotfile in this repo.
  homeFilesToLink = {
    ".emacs-profiles.el" = ./emacs-profiles.el;
    ".gvimrc" = ./gvimrc;
    ".hgrc.d/fancy.style" = ./hgrc.d/fancy.style;
    ".hgrc"   = ./hgrc;
    ".tmux.conf" = ./tmux.conf;
    ".nvim/after/ftplugin/org.vim" = ./vim/after/ftplugin/org.vim;
    ".nvim/bundle/Vundle.vim" = vundleRepoSrc;

    ".vimrc" = ./vimrc;
    ".vim/after/ftplugin/org.vim" = ./vim/after/ftplugin/org.vim;
    ".vim/bundle/Vundle.vim" = vundleRepoSrc;
  };

  # Function to help map attrs for symlinking home.file, xdg.configFile
  # e.g. from { ".hgrc" = ./hgrc; } to { ".hgrc".source = ./hgrc; }
  toSource = configDirName: dotfilesPath: { source = dotfilesPath; };
in
{
  # Symlink files under ~, e.g. ~/.hgrc
  home.file = pkgs.lib.attrsets.mapAttrs toSource homeFilesToLink;

  home.stateVersion = "22.05";

  programs.home-manager.enable = true;

  # Symlink files under ~/.config, e.g. ~/.config/alacritty/alacritty.yml
  xdg.configFile = pkgs.lib.attrsets.mapAttrs toSource configFilesToLink;
}
