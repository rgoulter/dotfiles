{ pkgs
, symlinkFromDir ? ./.
  # symlinksFromDir is the path to the 'root' directory
  # of where the files passed to functions in this lib
  # are symlinked from.
, ...
}:

rec {
  # e.g. given "alacritty/alacritty.yml",
  # return the attrset { "alacritty/alacritty.yml" = ./alacritty/alacritty.yml; }.
  genAttrsForSimpleLink = fileName: symlinkFromDir + ("/" + fileName);

  # e.g. given "hgrc"
  # return the attrset { ".hgrc" = ./hgrc; }.
  genAttrsForSimpleDotLink = fileName: { ".${fileName}" = symlinkFromDir + ("/" + fileName); };

  # Function to help map attrs for symlinking home.file, xdg.configFile
  # e.g. from { ".hgrc" = ./hgrc; } to { ".hgrc".source = ./hgrc; }
  toSource = configDirName: dotfilesPath: { source = dotfilesPath; };

  # Attribute set for dotfiles in this repo to link into ~/.config.
  # The attribute name is for ~/.config/$attrSetName,
  #  e.g. "alacritty/alacritty.yml" for ~/.config/alacritty/alacritty.yml
  # The attribute value is the path to the dotfile in this repo.
  #
  #   xdgConfigList = [
  #     "doom/config.el"
  #     "doom/init.el"
  #     "doom/packages.el"
  #     "fish/config.fish"
  #     "git/common.inc"
  #     "kitty/kitty.conf"
  #     "starship.toml"
  #     "tmux/tmux.conf"
  #   ];
  configFilesToLinkF = xdgConfigList:
    (pkgs.lib.attrsets.genAttrs xdgConfigList genAttrsForSimpleLink);

  # Attribute set for dotfiles in this repo to link into home directory.
  # The attribute name is for ~/$attrSetName,
  #  e.g. ".hgrc" for ~/.hgrc.
  # The attribute value is the path to the dotfile in this repo.
  #
  #   simpleHomeFilesToLinkList = [
  #     "hgrc"
  #     "hgrc.d/fancy.style"
  #     "vimrc"
  #   ];
  homeFilesToLinkF = simpleHomeFilesToLink:
    (pkgs.lib.lists.foldr (a: b: a // b) {} (map genAttrsForSimpleDotLink simpleHomeFilesToLink));

  mkSymlinkedDotfilesConfig = {
    # List of dotfiles where the path to link under
    # ~/.config/ matches the path in the dotfiles repo.
    # e.g. ~/.config/alacritty/alacritty.yml matches ./alacritty/alacritty.yml.
    simpleConfigFilesToLinkList ? [],

    # Files where the symlinks aren't following a nice convention.
    unconventionalConfigFilesToLink ? {},

    # e.g. "gvimrc" to link "~/.gvimrc" to ./gvimrc
    simpleHomeFilesToLinkList ? [],

    unconventionalHomeFilesToLink ? {},
  }:
  let
    # Attribute set for dotfiles in this repo to link into ~/.config.
    # The attribute name is for ~/.config/$attrSetName,
    #  e.g. "alacritty/alacritty.yml" for ~/.config/alacritty/alacritty.yml
    # The attribute value is the path to the dotfile in this repo.
    configFilesToLink =
      (configFilesToLinkF simpleConfigFilesToLinkList) //
      unconventionalConfigFilesToLink;

    # Attribute set for dotfiles in this repo to link into home directory.
    # The attribute name is for ~/$attrSetName,
    #  e.g. ".hgrc" for ~/.hgrc.
    # The attribute value is the path to the dotfile in this repo.
    homeFilesToLink =
      (homeFilesToLinkF simpleHomeFilesToLinkList) //
      unconventionalHomeFilesToLink;
  in
  {
    # Symlink files under ~, e.g. ~/.hgrc
    home.file = pkgs.lib.attrsets.mapAttrs toSource homeFilesToLink;
    # Symlink files under ~/.config, e.g. ~/.config/alacritty/alacritty.yml
    xdg.configFile = pkgs.lib.attrsets.mapAttrs toSource configFilesToLink;
  };
}

