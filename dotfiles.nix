{ config, pkgs, ... }:

let
  configSymlinksLib = import ./lib/configSymlinks.nix { inherit pkgs; symlinkFromDir = ./.; };
  sources = import ./lib/sources { inherit pkgs; };

  # List of dotfiles where the path to link under
  # ~/.config/ matches the path in the dotfiles repo.
  # e.g. ~/.config/alacritty/alacritty.yml matches ./alacritty/alacritty.yml.
  simpleConfigFilesToLinkList = [
    "alacritty/alacritty.yml"
    "chemacs/profile"
    "chemacs/profiles.el"
    "doom/config.el"
    "doom/init.el"
    "doom/packages.el"
    "emacs-rgoulter/init.el"
    "emacs-rgoulter/straight/versions/default.el"
    "fish/coloured-manpages.fish"
    "fish/config.fish"
    "fish/fishfile"
    "fish/functions/fish_greeting.fish"
    "fish/functions/fisher.fish"
    "fish/keybindings.txt"
    "git/common.inc"
    "git/gpg.inc"
    "kitty/kitty.conf"
    "powerline/themes/tmux/default.json"
    "starship.toml"
    "tmux/tmux.conf"
  ];

  # Files where the symlinks aren't following a nice convention.
  unconventionalConfigFilesToLink = {
    "emacs" = sources.chemacs2;
    "nvim/init.vim" = ./vimrc;
    "tmux/plugins/tpm" = sources.tpm;
  };

  # e.g. "gvimrc" to link "~/.gvimrc" to ./gvimrc
  simpleHomeFilesToLinkList = [
    "gvimrc"
    "hgrc"
    "hgrc.d/fancy.style"

    "vimrc"
    "vim/after/ftplugin/org.vim"
  ];

  unconventionalHomeFilesToLink = {
    ".nvim/after/ftplugin/org.vim" = ./vim/after/ftplugin/org.vim;
    ".nvim/bundle/Vundle.vim" = sources.vundle;
    ".vim/bundle/Vundle.vim" = sources.vundle;
  };

  # Attribute set for dotfiles in this repo to link into ~/.config.
  # The attribute name is for ~/.config/$attrSetName,
  #  e.g. "alacritty/alacritty.yml" for ~/.config/alacritty/alacritty.yml
  # The attribute value is the path to the dotfile in this repo.
  configFilesToLink =
    (configSymlinksLib.configFilesToLinkF simpleConfigFilesToLinkList) //
    unconventionalConfigFilesToLink;

  # Attribute set for dotfiles in this repo to link into home directory.
  # The attribute name is for ~/$attrSetName,
  #  e.g. ".hgrc" for ~/.hgrc.
  # The attribute value is the path to the dotfile in this repo.
  homeFilesToLink =
    (configSymlinksLib.homeFilesToLinkF simpleHomeFilesToLinkList) //
    unconventionalHomeFilesToLink;
in
{
  # Symlink files under ~, e.g. ~/.hgrc
  home.file = pkgs.lib.attrsets.mapAttrs configSymlinksLib.toSource homeFilesToLink;

  home.stateVersion = "22.05";

  programs.home-manager.enable = true;

  # Symlink files under ~/.config, e.g. ~/.config/alacritty/alacritty.yml
  xdg.configFile = pkgs.lib.attrsets.mapAttrs configSymlinksLib.toSource configFilesToLink;
}
