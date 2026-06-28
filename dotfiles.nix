{
  config,
  pkgs,
  lib,
  ...
}: let
  configSymlinksLib = import ./lib/configSymlinks.nix {inherit pkgs;};
  ensureClonedLib = import ./lib/ensureCloned.nix {inherit config lib pkgs;};

  # home-relative dest = git repo url
  ensureCloned = {
    ".config/emacs" = "https://github.com/plexus/chemacs2.git";
    ".config/tmux/plugins/tpm" = "https://github.com/tmux-plugins/tpm.git";
    ".vim/bundle/Vundle.vim" = "https://github.com/VundleVim/Vundle.vim.git";
  };

  # home-relative dest = home-relative src
  homeSymlinks = {
    ".nvim/bundle/Vundle.vim" = ".vim/bundle/Vundle.vim";
  };

  # List of dotfiles where the path to link under
  # ~/.config/ matches the path in the dotfiles repo.
  # e.g. ~/.config/alacritty/alacritty.yml matches ./alacritty/alacritty.yml.
  simpleConfigFilesToLinkList = [
    "alacritty/alacritty.yml"
    "chemacs/profile"
    "chemacs/profiles.el"
    "direnv/direnvrc"
    "direnv/direnv.toml"
    "doom/config.el"
    "doom/init.el"
    "doom/lisp/agent-shell-grok.el"
    "doom/packages.el"
    "emacs-rgoulter/init.el"
    "emacs-rgoulter/straight/versions/default.el"
    "fish/coloured-manpages.fish"
    "fish/config.fish"
    # "fish/fishfile"
    # "fish/functions/fisher.fish"
    "fish/keybindings.txt"
    "ghostty/config"
    "ghostty/themes/gruvbox-dark"
    "ghostty/themes/gruvbox-light"
    "git/common.inc"
    "git/config.example"
    "git/gpg.inc"
    "git/ignore"
    "helix/config.toml"
    "shell/gpg-agent.envsh"
    "shell/source-nix-daemon.sh"
    "kitty/dark-theme.auto.conf"
    "kitty/kitty.conf"
    "kitty/light-theme.auto.conf"
    "kitty/no-preference-theme.auto.conf"
    "nvim/init.vim"
    "powerline/themes/tmux/default.json"
    "starship.toml"
    "tmux/tmux.conf"
  ];

  # e.g. "gvimrc" to link "~/.gvimrc" to ./gvimrc
  simpleHomeFilesToLinkList = [
    "gvimrc"
    "hgrc"
    "hgrc.d/fancy.style"
    "ssh/config"
    "vimrc"
    "vim/after/ftplugin/org.vim"
  ];

  extraHomeFileLinks = {
    ".nvim/after/ftplugin/org.vim" = ./vim/after/ftplugin/org.vim;
  };

  symlinkedConfig = configSymlinksLib.mkSymlinkedDotfilesConfig {
    inherit
      simpleConfigFilesToLinkList
      simpleHomeFilesToLinkList
      extraHomeFileLinks
      ;
    symlinkFromDir = ./.;
  };
in {
  imports = [./themes.nix];

  dotfiles.themes.enable = true;

  home.file = symlinkedConfig.home.file;

  xdg.configFile = lib.mkMerge [
    symlinkedConfig.xdg.configFile
    {
      # Managed as a real file in themes.nix so Zellij can hot-reload it.
      "zellij/config.kdl".enable = false;
    }
  ];

  home.activation = ensureClonedLib.mkActivation {
    inherit ensureCloned homeSymlinks;
  };
}
