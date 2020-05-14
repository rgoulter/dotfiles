self: super:

{
  myPackages = super.buildEnv {
    name = "my-packages";
    paths = with self; [
      awscli
      bat
      chromedriver
      curlie
      docker
      emacs
      emacs-all-the-icons-fonts
      fd
      fish
      fzf
      gcc
      git
      gnupg
      go
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
      vscode
      websocat
      yarn
    ];
    pathsToLink = [ "/share" "/bin" ];
  };
}
