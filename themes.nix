{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.dotfiles.themes;
  zellijConfig = ./zellij/config.kdl;
in {
  options.dotfiles.themes = {
    enable = lib.mkEnableOption "OS-driven Gruvbox theme sync for Kitty, Zellij, Helix, Grok, and pi";
  };

  config = lib.mkIf cfg.enable {
    home.file.".grok/config.toml" = {
      source = ./grok/config.toml;
      force = true;
    };

    # pi-coding-agent: ANSI themes inherit Kitty palette; extension polls OS appearance.
    home.file.".pi/agent/themes/ansi-dark.json".source = ./pi/agent/themes/ansi-dark.json;
    home.file.".pi/agent/themes/ansi-light.json".source = ./pi/agent/themes/ansi-light.json;
    home.file.".pi/agent/extensions/system-theme.ts".source = ./pi/agent/extensions/system-theme.ts;

    # Zellij ignores inotify events on symlinked configs, so install a real file.
    home.activation.installZellijConfig = lib.hm.dag.entryAfter ["writeBoundary"] ''
      $DRY_RUN_CMD mkdir -p "${config.home.homeDirectory}/.config/zellij"
      $DRY_RUN_CMD install -m644 ${zellijConfig} "${config.home.homeDirectory}/.config/zellij/config.kdl"
    '';
  };
}