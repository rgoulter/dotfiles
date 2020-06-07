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

let
  ginkgo =
    with super;
    buildGoModule rec {
      pname = "ginkgo";
      version = "1.12.2";

      src = fetchFromGitHub {
        owner = "onsi";
        repo = "ginkgo";
        rev = "v${version}";
        sha256 = "1c2g63yblzdz5b27403vvj6n3f6sl9zdwq7r6k0zwqhbc4afb9fi";
      };

      vendorSha256 = "072amyw1ir18v9vk268j2y7dhw3lfwvxzvzsdqhnp50rxsa911bx";

      subPackages = [ "ginkgo" ];

      meta = with stdenv.lib; {
        description = "Simple command-line snippet manager, written in Go";
        homepage = "http://onsi.github.io/ginkgo/";
        license = licenses.mit;
        maintainers = [ "onsi" ];
        platforms = platforms.linux ++ platforms.darwin;
      };
    };
in
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
      ginkgo
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
