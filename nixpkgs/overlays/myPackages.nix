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
      aws-mfa
      babelfish
      bash
      bat
      bottom
      coreutils
      ctags
      direnv
      docker
      emacs
      emacs-all-the-icons-fonts
      exa
      fd
      fish
      fzf
      fzy
      gawk
      git
      gitAndTools.tig
      glances
      gnupg
      google-cloud-sdk
      htop
      jq
      k9s
      kakoune
      keepassxc
      kitty
      kubectl
      lazydocker
      lazygit
      less
      neovim
      openssh
      procps
      python38Packages.powerline
      ripgrep
      # ruby needed for tmux-jump
      ruby
      safe
      silver-searcher
      skim
      starship
      tldr
      tmate
      tmux
      tree
      vim
      wget
      which
    ] ++ (lib.optionals true [
      # Unfree software; requires config.allowUnfree = true
      vscode
    ]) ++(lib.optionals stdenv.isDarwin [
      pinentry_mac
    ]) ++ (lib.optionals stdenv.isLinux [
      desktop-file-utils
      emacsWithProfileDoomApplication
      emacsWithProfileSpacemacsApplication
      (firefox.override {
        cfg = {
          enableTridactylNative = true;
        };
      })
      google-chrome
      lens
      obs-studio
      onedrive
      pinentry_gtk2
      slack
      spotify
      tdesktop
      vlc
    ]);
    pathsToLink = ["/bin" "/lib" "/share" ] ++ (with self; lib.optionals stdenv.isDarwin [ "/Applications" "/Library" ]);
  };
}

