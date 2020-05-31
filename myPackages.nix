# An overlay which adds myPackages for declarative package management.
#
# Put this file in ~/.config/nixpkgs/overlays/
# and run `nix-env --install --attr nixpkgs.myPackages
#
# Some of the packages in myPackages are unfree.
# ~/.config/nix/config.nix should have allowUnfree = true.
#
# See:
# - https://nixos.org/nixpkgs/manual/#sec-declarative-package-management
# - https://nixos.org/nixpkgs/manual/#chap-overlays
self: super:

{
  myPackages = super.buildEnv {
    name = "my-packages";
    paths = with self; [
      alacritty
      aspell
      aspellDicts.en
      aspellDicts.en-computers
      aspellDicts.vi
      awscli
      bat
      chromedriver
      ctags
      curlie
      docker
      emacs
      emacs-all-the-icons-fonts
      fd
      fish
      fzf
      fzy
      gcc
      geckodriver
      git
      gitAndTools.tig
      glances
      gnupg
      go
      gopls
      gron
      groovy
      htop
      jid
      jl
      jq
      jsawk
      keepassxc
      lazydocker
      lnav
      mc
      neovim
      nodejs
      nox
      openssh
      plantuml
      python3
      ripgrep
      ruby
      rustup
      scc
      shellcheck
      silver-searcher
      skim
      stack
      starship
      terraform
      tldr
      tmux
      tokei
      tree
      up
      vim
      vscode
      websocat
      yarn
    ];
    pathsToLink = [ "/share" "/bin" "/lib" ];
  };
}
