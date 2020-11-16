# Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

let
  hostSpecific = import ./host-specific.nix;
in
{
  boot = hostSpecific.boot;
  networking = hostSpecific.networking;

  environment.systemPackages = with pkgs; [
    ksshaskpass
  ];

  hardware.bluetooth.enable = true;

  imports =
    [
      ./hardware-configuration.nix
    ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  #   pinentryFlavor = "gnome3";
  # };
  programs.ssh.startAgent = true;

  security.pam.services.kwallet = {
    name = "kwallet";
    enableKwallet = true;
  };

  services = {
    # Enable the OpenSSH daemon.
    openssh.enable = true;

    udev.packages = [ pkgs.yubikey-personalization ];

    # Enable the X11 windowing system.
    xserver = {
      # Enable the KDE Desktop Environment.
      desktopManager.plasma5.enable = true;

      # Enable the KDE's Display Manager
      displayManager.sddm.enable = true;

      enable = true;

      layout = "us";
    };
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.03"; # Did you read the comment?

  # Set your time zone.
  time.timeZone = "Asia/Jakarta";

  users.users.rgoulter = {
    isNormalUser = true;
    extraGroups = [
      "docker"
      "networkmanager"
      "wheel"           # Enable ‘sudo’ for the user.
    ];
  };

  virtualisation.docker.enable = true;
}
