{config, lib, pkgs}: let
  home = config.home.homeDirectory;

  activationName = prefix: path: "${prefix}-${lib.replaceStrings ["/" "."] ["-" "-"] path}";

  mkEnsureCloned = ensureCloned:
    lib.mapAttrs' (dest: url: {
      name = activationName "ensureCloned" dest;
      value = lib.hm.dag.entryAfter ["writeBoundary"] ''
        repoPath=${lib.escapeShellArg "${home}/${dest}"}
        $DRY_RUN_CMD mkdir -p "$(dirname "$repoPath")"
        if [ -e "$repoPath" ] && [ ! -d "$repoPath/.git" ]; then
          $DRY_RUN_CMD rm -rf "$repoPath"
        fi
        if [ ! -d "$repoPath/.git" ]; then
          $DRY_RUN_CMD ${pkgs.git}/bin/git clone ${lib.escapeShellArg url} "$repoPath"
        fi
      '';
    })
    ensureCloned;

  mkHomeSymlinks = homeSymlinks:
    lib.mapAttrs' (dest: src: {
      name = activationName "homeSymlink" dest;
      value = lib.hm.dag.entryAfter ["writeBoundary"] ''
        destPath=${lib.escapeShellArg "${home}/${dest}"}
        srcPath=${lib.escapeShellArg "${home}/${src}"}
        $DRY_RUN_CMD mkdir -p "$(dirname "$destPath")"
        $DRY_RUN_CMD ln -sfn "$srcPath" "$destPath"
      '';
    })
    homeSymlinks;

  mkActivation = {
    ensureCloned ? {},
    homeSymlinks ? {},
  }:
    (mkEnsureCloned ensureCloned) // (mkHomeSymlinks homeSymlinks);
in {
  inherit mkActivation;
}
