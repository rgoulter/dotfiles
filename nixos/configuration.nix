# Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

let
  hostSpecific = import ./host-specific.nix;
in
{
  boot = hostSpecific.boot;
  networking = hostSpecific.networking;

  hardware.bluetooth.enable = true;
  hardware.pulseaudio.enable = true;

  imports =
    [
      ./hardware-configuration.nix
    ];

  nixpkgs.config.allowUnfree = true;

  programs.ssh.startAgent = true;

  services = {
    # Enable the OpenSSH daemon.
    openssh.enable = true;

    udev = {
      # https://docs.qmk.fm/#/faq_build?id=linux-udev-rules
      extraRules = ''
# Atmel DFU
SUBSYSTEMS=="usb", ATTRS{idVendor}=="03eb", ATTRS{idProduct}=="2ff4", TAG+="uaccess"
      '';
      packages = [ pkgs.yubikey-personalization ];
    };

    # Enable the X11 windowing system.
    xserver = {
      # Enable the GNOME 3 Desktop Environment.
      desktopManager.gnome.enable = true;
      displayManager.gdm.enable = true;


      enable = true;

      videoDrivers = [ "nvidia" ];

      layout = "us";
    };
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.05";

  time.timeZone = "Asia/Jakarta";

  users.users.rgoulter = {
    isNormalUser = true;
    extraGroups = [
      "audio"
      "docker"
      "libvirtd"
      "networkmanager"
      "wheel"
    ];
  };
  users.extraGroups.vboxusers.members = ["rgoulter"];

  virtualisation = {
    docker.enable = true;
    libvirtd.enable = true;
    virtualbox.host.enable = true;
  };
}
