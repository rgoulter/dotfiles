# Adapted from
# <nixpkgs>/pkgs/applications/networking/cluster/terraform/default.nix

{ stdenv, lib, buildEnv, buildGoPackage, fetchFromGitHub, makeWrapper, coreutils
, runCommand, writeText, terraform-providers, fetchpatch }:

let
  goPackagePath = "github.com/hashicorp/terraform";

  generic = { version, sha256, ... }@attrs:
    let attrs' = builtins.removeAttrs attrs [ "version" "sha256" ];
    in buildGoPackage ({
      name = "terraform-${version}";

      inherit goPackagePath;

      src = fetchFromGitHub {
        owner = "hashicorp";
        repo = "terraform";
        rev = "v${version}";
        inherit sha256;
      };

      postPatch = ''
        # speakeasy hardcodes /bin/stty https://github.com/bgentry/speakeasy/issues/22
        substituteInPlace vendor/github.com/bgentry/speakeasy/speakeasy_unix.go \
          --replace "/bin/stty" "${coreutils}/bin/stty"
      '';

      postInstall = ''
        # remove all plugins, they are part of the main binary now
        for i in $out/bin/*; do
          if [[ $(basename $i) != terraform ]]; then
            rm "$i"
          fi
        done
      '';

      preCheck = ''
        export HOME=$TMP
      '';

      meta = with lib; {
        description =
          "Tool for building, changing, and versioning infrastructure";
        homepage = "https://www.terraform.io/";
        license = licenses.mpl20;
        maintainers = with maintainers; [
          zimbatm
          peterhoeg
          kalbasit
          marsam
          babariviere
        ];
      };
    } // attrs');

  pluggable = terraform:
    let
      withPlugins = plugins:
        let
          actualPlugins = plugins terraform.plugins;

          # Wrap PATH of plugins propagatedBuildInputs, plugins may have runtime dependencies on external binaries
          wrapperInputs = lib.unique (lib.flatten
            (lib.catAttrs "propagatedBuildInputs"
              (builtins.filter (x: x != null) actualPlugins)));

          passthru = {
            withPlugins = newplugins:
              withPlugins (x: newplugins x ++ actualPlugins);
            full = withPlugins lib.attrValues;

            # Ouch
            overrideDerivation = f:
              (pluggable (terraform.overrideDerivation f)).withPlugins plugins;
            overrideAttrs = f:
              (pluggable (terraform.overrideAttrs f)).withPlugins plugins;
            override = x:
              (pluggable (terraform.override x)).withPlugins plugins;
          };
          # Don't bother wrapping unless we actually have plugins, since the wrapper will stop automatic downloading
          # of plugins, which might be counterintuitive if someone just wants a vanilla Terraform.
        in if actualPlugins == [ ] then
          terraform.overrideAttrs
          (orig: { passthru = orig.passthru // passthru; })
        else
          lib.appendToName "with-plugins" (stdenv.mkDerivation {
            inherit (terraform) name;
            buildInputs = [ makeWrapper ];

            buildCommand = ''
              mkdir -p $out/bin/
              makeWrapper "${terraform}/bin/terraform" "$out/bin/terraform" \
                --set NIX_TERRAFORM_PLUGIN_DIR "${
                  buildEnv {
                    name = "tf-plugin-env";
                    paths = actualPlugins;
                  }
                }/bin" \
                --prefix PATH : "${lib.makeBinPath wrapperInputs}"
            '';

            inherit passthru;
          });
    in withPlugins (_: [ ]);

  plugins = removeAttrs terraform-providers [
    "override"
    "overrideDerivation"
    "recurseForDerivations"
  ];
in rec {
  terraform_0_13 = pluggable (generic {
    version = "0.13.3";
    sha256 = "1bjgsabkyh3xf4gdp41mwhw6bpmjg45bnq2hk522glk02n2xngpw";
    passthru = { inherit plugins; };
  });

  terraform_0_13-full = terraform_0_13.full;
}
