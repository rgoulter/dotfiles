self: super:

{
  myPackages = super.buildEnv {
    name = "my-packages";
    paths = with self; [
      alacritty
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
      silver-searcher
      skim
      stack
      starship
      terraform
      tree
      tldr
      tmux
      tokei
      up
      vim
      vscode
      websocat
      yarn
    ];
    pathsToLink = [ "/share" "/bin" ];
  };
}
