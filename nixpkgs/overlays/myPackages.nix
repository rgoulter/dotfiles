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
  anki = super.anki.overrideAttrs (oldAttrs: { doInstallCheck = false; doCheck = false; });
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

      meta = with lib; {
        description = "Simple command-line snippet manager, written in Go";
        homepage = "http://onsi.github.io/ginkgo/";
        license = licenses.mit;
        maintainers = [ "onsi" ];
        platforms = platforms.linux ++ platforms.darwin;
      };
    };
  kind =
    with super;
    buildGoModule rec {
      pname = "kind";
      version = "0.9.0";

      src = fetchFromGitHub {
        owner = "kubernetes-sigs";
        repo = "kind";
        rev = "v${version}";
        sha256 = "1kyjmlp1kmr3lwylnya6w392j1qpqgbvcacwpnz3ifyh3pbv32qr";
      };

      vendorSha256 = "04fmqh6lhvvzpvf1l2xk1r8687k5jx2lb5199rgmjbfnjgsa0q2d";

      subPackages = [ "cmd/kind" ];

      meta = with lib; {
        description = "Kubernetes IN Docker - local clusters for testing Kubernetes ";
        homepage = "https://kind.sigs.k8s.io/";
        license = licenses.asl20;
        maintainers = [ "kubernetes-sigs" ];
        platforms = platforms.linux ++ platforms.darwin;
      };
    };
  swag =
    with super;
    buildGoModule rec {
      pname = "swaggo-swag";
      version = "1.6.9";

      src = pkgs.fetchFromGitHub {
        owner = "swaggo";
        repo = "swag";
        rev = "v${version}";
        sha256 = "07wx9sbca4vamy8ywhj1lr9bj6j7rscfw8v0dsyvzysibsp7abjk";
      };

      vendorSha256 = "0xi0hyifqg7d5f1dzqhaf1p6yylywh7rxd0vsx5y0a8x7bib4hgi";

      subPackages = [ "cmd/swag" ];

      meta = with pkgs.lib; {
        description = "Automatically generate RESTful API documentation with Swagger 2.0 for Go.";
        homepage = "https://github.com/swaggo/swag/";
        license = "licenses.mit";
        maintainers = with maintainers; [ "rgoulter" ];
        platforms = platforms.linux ++ platforms.darwin;
      };
    };
  emacsWithProfileDoomApplication =
    super.writeTextFile {
      name = "chemacs-doom";
      destination = "/share/applications/emacs-doom.desktop";
      text = ''
[Desktop Entry]
Name=Emacs (Doom)
GenericName=Text Editor
Comment=Edit text
MimeType=text/english;text/plain;text/x-makefile;text/x-c++hdr;text/x-c++src;text/x-chdr;text/x-csrc;text/x-java;text/x-moc;text/x-pascal;text/x-tcl;text/x-tex;application/x-shellscript;text/x-c;text/x-c++;
Exec=emacs --with-profile doom %F
Icon=emacs
Type=Application
Terminal=false
Categories=Development;TextEditor;
StartupWMClass=Emacs
Keywords=Text;Editor;
      '';
        };
  emacsWithProfileSpacemacsApplication =
    super.writeTextFile {
      name = "chemacs-spacemacs";
      destination = "/share/applications/emacs-spacemacs.desktop";
      text = ''
[Desktop Entry]
Name=Emacs (Spacemacs)
GenericName=Text Editor
Comment=Edit text
MimeType=text/english;text/plain;text/x-makefile;text/x-c++hdr;text/x-c++src;text/x-chdr;text/x-csrc;text/x-java;text/x-moc;text/x-pascal;text/x-tcl;text/x-tex;application/x-shellscript;text/x-c;text/x-c++;
Exec=emacs --with-profile spacemacs %F
Icon=emacs
Type=Application
Terminal=false
Categories=Development;TextEditor;
StartupWMClass=Emacs
Keywords=Text;Editor;
      '';
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

  ##     meta = with pkgs.lib; {
  ##       description = "MockGen generates mock implementations of Go interfaces.";
  ##       homepage = "https://github.com/golang/mock/";
  ##       license = licenses.asl20;
  ##       maintainers = [];
  ##       platforms = platforms.linux ++ platforms.darwin;
  ##     };
  ##   };
in
{
  python3 = super.python3.override {
      packageOverrides = self: super: {
        crate = super.crate.overrideAttrs (oldAttrs: { doInstallCheck = false; doCheck = false; });
      };
    };
  myPackages = super.buildEnv {
    name = "my-packages";
    paths = with self; [
      alacritty
      anki
      aspell
      aspellDicts.en
      aspellDicts.en-computers
      aspellDicts.vi
      awscli2
      aws_mfa
      bash
      bat
      bottom
      chromedriver
      coreutils
      csvkit
      ctags
      curlie
      direnv
      docker
      eksctl
      emacs
      emacs-all-the-icons-fonts
      exa
      fd
      fish
      fzf
      fzy
      gawk
      gcc
      geckodriver
      git
      gitAndTools.tig
      ginkgo
      glances
      gnupg
      go
      google-cloud-sdk
      gopls
      gron
      groovy
      htop
      imagemagick
      jid
      jl
      jq
      # jsawk # the given spidermonkey version is insecure
      k9s
      kakoune
      keepassxc
      kind
      kitty
      kubectl
      lazydocker
      lazygit
      less
      lnav
      mc
      minikube
      mockgen
      neovim
      nodejs
      nox
      openssh
      perl
      plantuml
      procps
      python3
      python38Packages.powerline
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
      stlink
      terraform_0_13
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
      websocat
      which
      yarn
    ] ++ (lib.optionals true [
      # Unfree software; requires config.allowUnfree = true
      vscode
    ]) ++(lib.optionals stdenv.isDarwin [
      pinentry_mac
    ]) ++ (lib.optionals stdenv.isLinux [
      emacsWithProfileDoomApplication
      emacsWithProfileSpacemacsApplication
      firefox
      google-chrome
      obs-studio
      onedrive
      lens
      pinentry_gtk2
    ]);
    pathsToLink = ["/bin" "/lib" "/share" ] ++ (with self; lib.optionals stdenv.isDarwin [ "/Applications" "/Library" ]);
  };
}
