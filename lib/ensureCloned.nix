{
  config,
  lib,
  pkgs,
}: let
  home = config.home.homeDirectory;

  # "https://…" or { src = "https://…"; rev = "…"; }
  parseCloneSpec = spec:
    if builtins.isString spec
    then {
      src = spec;
      rev = null;
    }
    else {
      src = spec.src;
      rev = spec.rev or null;
    };

  activationName = prefix: path: "${prefix}-${lib.replaceStrings ["/" "."] ["-" "-"] path}";

  mkEnsureCloned = ensureCloned:
    lib.mapAttrs' (dest: spec: let
      inherit (parseCloneSpec spec) src rev;
      repoPath = "${home}/${dest}";
    in {
      name = activationName "ensureCloned" dest;
      value = lib.hm.dag.entryAfter ["writeBoundary"] ''
        repoPath=${lib.escapeShellArg repoPath}
        $DRY_RUN_CMD mkdir -p "$(dirname "$repoPath")"
        if [ -e "$repoPath" ] && [ ! -d "$repoPath/.git" ]; then
          $DRY_RUN_CMD rm -rf "$repoPath"
        fi
        if [ ! -d "$repoPath/.git" ]; then
          $DRY_RUN_CMD ${pkgs.git}/bin/git clone ${lib.escapeShellArg src} "$repoPath"
          ${lib.optionalString (rev != null) ''
          $DRY_RUN_CMD ${pkgs.git}/bin/git -C "$repoPath" checkout ${lib.escapeShellArg rev}
        ''}
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
