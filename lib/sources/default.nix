{pkgs, ...}: {
  chemacs2 = pkgs.fetchFromGitHub (pkgs.lib.importJSON ./plexus-chemacs2.json);
  tpm =
    pkgs.fetchFromGitHub (pkgs.lib.importJSON ./tmux-plugins-tpm.json);
  # Using the submodule in this dotfiles repo would make
  # require a more awkward flake URI.
  vundle = pkgs.fetchFromGitHub (pkgs.lib.importJSON ./vundlevim-vundle.vim.json);
}
