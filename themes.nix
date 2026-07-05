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
    enable = lib.mkEnableOption "OS-driven Gruvbox theme sync for Kitty, Ghostty, Zellij, Helix, Grok, and pi";
  };

  config = lib.mkIf cfg.enable {
    home.file.".grok/config.toml" = {
      source = ./grok/config.toml;
      force = true;
    };

    home.file.".grok/sandbox.toml" = {
      text = ''
        # Managed by Home Manager (dotfiles/themes.nix). Grok applies at session start.

        [profiles.personal]
        extends = "workspace"
        read_write = [
          "${config.home.homeDirectory}/github",
        ]
      '';
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

    # Pre-auto-theme symlink; overrides palette and breaks CSI 2031 propagation.
    home.activation.removeStaleKittyThemeConf = lib.hm.dag.entryAfter ["writeBoundary"] ''
      if [ -L "${config.home.homeDirectory}/.config/kitty/theme.conf" ] \
         || [ -f "${config.home.homeDirectory}/.config/kitty/theme.conf" ]; then
        $DRY_RUN_CMD rm -f "${config.home.homeDirectory}/.config/kitty/theme.conf"
      fi
    '';
  };
}
