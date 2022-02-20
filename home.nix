{ config, pkgs, ... }:

{
  home.stateVersion = "22.05";

  programs.home-manager.enable = true;

  xdg.configFile."alacritty/alacritty.yml".source = ./alacritty.yml;
  xdg.configFile."emacs-rgoulter/init.el".source  = ./emacs.el;
  xdg.configFile."emacs-rgoulter/straight/versions/default.el".source  = ./emacs.d/straight/versions/default.el;
  xdg.configFile."fish/coloured-manpages.fish".source  = ./fish/coloured-manpages.fish;
  xdg.configFile."fish/config.fish".source  = ./fish/config.fish;
  xdg.configFile."fish/fishfile".source     = ./fish/fishfile;
  xdg.configFile."fish/keybindings.txt".source        = ./fish/keybindings.txt;
  xdg.configFile."fish/functions/fisher.fish".source  = ./fish/functions/fisher.fish;
  xdg.configFile."fish/functions/fish_greeting.fish".source  = ./fish/functions/fish_greeting.fish;
  xdg.configFile."git/common.inc".source   = ./git/common.inc;

  xdg.configFile."kitty/kitty.conf".source = ./kitty/kitty.conf;
  xdg.configFile."powerline/themes/tmux/default.json".source = ./powerline/themes/tmux/default.json;
  xdg.configFile."nvim/init.vim".source = ./vimrc;
  xdg.configFile."starship.toml".source = ./starship.toml;

  home.file.".emacs-profiles.el".source = ./emacs-profiles.el;
  home.file.".gvimrc".source = ./gvimrc;
  home.file.".hgrc.d/fancy.style".source = ./hgrc.d/fancy.style;
  home.file.".hgrc".source   = ./hgrc;
  home.file.".tmux.conf".source = ./tmux.conf;

  home.file.".nvim/after/ftplugin/org.vim".source = ./vim/after/ftplugin/org.vim;
  # Using the submodule in this dotfiles repo would make
  # require a more awkward flake URI.
  home.file.".nvim/bundle/Vundle.vim".source = pkgs.fetchFromGitHub {
    owner = "VundleVim";
    repo = "Vundle.vim";
    rev = "cfd3b2d388a8c2e9903d7a9d80a65539aabfe933";
    sha256 = "sha256-OCCXgMVWj/aBWLGaZmMr+cD546+QgynmEN/ECp1r08Q=";
  };

  home.file.".vimrc".source = ./vimrc;
  home.file.".vim/after/ftplugin/org.vim".source = ./vim/after/ftplugin/org.vim;
  home.file.".vim/bundle/Vundle.vim".source = pkgs.fetchFromGitHub {
    owner = "VundleVim";
    repo = "Vundle.vim";
    rev = "cfd3b2d388a8c2e9903d7a9d80a65539aabfe933";
    sha256 = "sha256-OCCXgMVWj/aBWLGaZmMr+cD546+QgynmEN/ECp1r08Q=";
  };
}
