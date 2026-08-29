{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.dotfiles.grok;
  home = config.home.homeDirectory;
  sandboxToml =
    builtins.replaceStrings ["@HOME@"] [home] (builtins.readFile ./grok/sandbox.toml);
  sandboxTomlFile = pkgs.writeText "grok-sandbox.toml" sandboxToml;
  configTomlFile = pkgs.writeText "grok-config.toml" (builtins.readFile ./grok/config.toml);
in {
  options.dotfiles.grok = {
    enable = lib.mkEnableOption "Grok CLI config and sandbox profile";
  };

  config = lib.mkIf cfg.enable {
    # Grok refuses custom sandbox profiles when ~/.grok/{config,sandbox}.toml
    # are symlinks (hook write-deny / retargeting check). Home Manager has no
    # home.file.*.copy, so install regular files instead of linking.
    home.activation.grokToml = lib.hm.dag.entryAfter ["writeBoundary"] ''
      run mkdir -p "$HOME/.grok"
      run ${lib.getExe' pkgs.coreutils "install"} -m 644 ${configTomlFile} "$HOME/.grok/config.toml"
      run ${lib.getExe' pkgs.coreutils "install"} -m 644 ${sandboxTomlFile} "$HOME/.grok/sandbox.toml"
    '';
  };
}
