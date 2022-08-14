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
}
