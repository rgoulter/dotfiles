{
  config,
  lib,
  ...
}: let
  cfg = config.dotfiles.grok;
  home = config.home.homeDirectory;
  sandboxToml =
    builtins.replaceStrings ["@HOME@"] [home] (builtins.readFile ./grok/sandbox.toml);
in {
  options.dotfiles.grok = {
    enable = lib.mkEnableOption "Grok CLI config and sandbox profile";
  };

  config = lib.mkIf cfg.enable {
    home.file.".grok/config.toml" = {
      source = ./grok/config.toml;
      force = true;
    };

    home.file.".grok/sandbox.toml" = {
      text = sandboxToml;
      force = true;
    };
  };
}
