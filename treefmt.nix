{pkgs, ...}: {
  projectRootFile = "flake.nix";
  programs.alejandra.enable = true;
  programs.fish_indent.enable = true;
  programs.kdlfmt.enable = true;
  programs.shellcheck.enable = true;
  programs.shfmt.enable = true;
  programs.taplo.enable = true;
  programs.yamlfmt.enable = true;

  settings.formatter.kdlfmt.options = let
    kdlfmtConfig = pkgs.writeText "kdlfmt.kdl" ''
      indent_size 2
      use_tabs false
    '';
  in [
    "--config"
    "${kdlfmtConfig}"
  ];
}
