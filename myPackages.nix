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
      gron
      groovy
      htop
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
      silver-searcher
      skim
      stack
      starship
      terraform
      tree
      tldr
      tmux
      up
      vim
      vscode
      websocat
      yarn
    ];
    pathsToLink = [ "/share" "/bin" ];
  };
}
