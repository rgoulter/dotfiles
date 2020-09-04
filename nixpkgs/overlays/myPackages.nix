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
  ## mockgen =
  ##   with super;
  ##   buildGoModule rec {
  ##     pname = "go-mock";
  ##     version = "1.4.4";

  ##     src = pkgs.fetchFromGitHub {
  ##       owner = "golang";
  ##       repo = "mock";
  ##       rev = "v${version}";
  ##       sha256 = "1lj0dvd6div4jaq1s0afpwqaq9ah8cxhkq93wii2ably1xmp2l0a";
  ##     };

  ##     vendorSha256 = "1md4cg1zzhc276sc7i2v0xvg5pf6gzy0n9ga2g1lx3d572igq1wy";

  ##     subPackages = [ "mockgen" ];

  ##     preBuild = ''
  ##       export buildFlagsArray+=("--ldflags" "-X main.version=1.4.3")
  ##     '';

  ##     meta = with pkgs.stdenv.lib; {
  ##       description = "MockGen generates mock implementations of Go interfaces.";
  ##       homepage = "https://github.com/golang/mock/";
  ##       license = licenses.asl20;
  ##       maintainers = [];
  ##       platforms = platforms.linux ++ platforms.darwin;
  ##     };
  ##   };
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
      awscli2
      aws_mfa
      bat
      chromedriver
      coreutils
      ctags
      curlie
      docker
      eksctl
      emacs
      emacs-all-the-icons-fonts
      exa
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
      kubectl
      lazydocker
      less
      lnav
      mc
      mockgen
      neovim
      nodejs
      nox
      openssh
      perl
      plantuml
      procps
      python3
      ripgrep
      ruby
      rustup
      safe
      scc
      shellcheck
      silver-searcher
      skim
      stack
      starship
      terraform
      terragrunt
      tflint
      tfsec
      tldr
      tmate
      tmux
      tokei
      tree
      up
      vagrant
      vault
      vaultenv
      vim
      vscode
      websocat
      which
      yarn
      ytop
    ];
    pathsToLink = [ "/share" "/bin" "/lib" ] ++ (if self.stdenv.isDarwin then [ "/Applications" "/Library" ] else []);
  };
}
