{ config, pkgs, ... }:

let
  chemacs2 = pkgs.fetchFromGitHub (pkgs.lib.importJSON ./plexus-chemacs2.json);

  tpm =
    pkgs.fetchFromGitHub (pkgs.lib.importJSON ./tmux-plugins-tpm.json);

  # Using the submodule in this dotfiles repo would make
  # require a more awkward flake URI.
  vundleRepoSrc = pkgs.fetchFromGitHub (pkgs.lib.importJSON ./vundlevim-vundle.vim.json);

  # e.g. given "alacritty/alacritty.yml",
  # return the attrset { "alacritty/alacritty.yml" = ./alacritty/alacritty.yml; }.
  genAttrsForSimpleLink = fileName: ./. + ("/" + fileName);

  # e.g. given "hgrc"
  # return the attrset { ".hgrc" = ./hgrc; }.
  genAttrsForSimpleDotLink = fileName: { ".${fileName}" = ./. + ("/" + fileName); };

  # Function to help map attrs for symlinking home.file, xdg.configFile
  # e.g. from { ".hgrc" = ./hgrc; } to { ".hgrc".source = ./hgrc; }
  toSource = configDirName: dotfilesPath: { source = dotfilesPath; };

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
    "kitty/kitty.conf"
    "powerline/themes/tmux/default.json"
    "starship.toml"
    "tmux/tmux.conf"
  ];

  # Files where the symlinks aren't following a nice convention.
  unconventionalConfigFilesToLink = {
    "emacs" = chemacs2;
    "nvim/init.vim" = ./vimrc;
    "tmux/plugins/tpm" = tpm;
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
    ".nvim/bundle/Vundle.vim" = vundleRepoSrc;
    ".vim/bundle/Vundle.vim" = vundleRepoSrc;
  };

  # Attribute set for dotfiles in this repo to link into ~/.config.
  # The attribute name is for ~/.config/$attrSetName,
  #  e.g. "alacritty/alacritty.yml" for ~/.config/alacritty/alacritty.yml
  # The attribute value is the path to the dotfile in this repo.
  configFilesToLink =
    (pkgs.lib.attrsets.genAttrs simpleConfigFilesToLinkList genAttrsForSimpleLink) //
    unconventionalConfigFilesToLink;

  # Attribute set for dotfiles in this repo to link into home directory.
  # The attribute name is for ~/$attrSetName,
  #  e.g. ".hgrc" for ~/.hgrc.
  # The attribute value is the path to the dotfile in this repo.
  homeFilesToLink =
    (pkgs.lib.lists.foldr (a: b: a // b) {} (map genAttrsForSimpleDotLink simpleHomeFilesToLinkList)) //
    unconventionalHomeFilesToLink;
in
{
  # Symlink files under ~, e.g. ~/.hgrc
  home.file = pkgs.lib.attrsets.mapAttrs toSource homeFilesToLink;

  home.stateVersion = "22.05";

  programs.home-manager.enable = true;

  # Symlink files under ~/.config, e.g. ~/.config/alacritty/alacritty.yml
  xdg.configFile = pkgs.lib.attrsets.mapAttrs toSource configFilesToLink;
}
