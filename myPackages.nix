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
      gcc
      geckodriver
      git
      gitAndTools.tig
      glances
      gnupg
      go
      htop
      jq
      keepassxc
      lazydocker
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
      stack
      starship
      terraform
      tldr
      tmux
      vim
      vscode
      websocat
      yarn
    ];
    pathsToLink = [ "/share" "/bin" ];
  };
}
