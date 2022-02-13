# Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [
      ./hardware-configuration.nix
      ./host-specific.nix
    ];

  # https://nixos.wiki/wiki/Bluetooth#Using_Bluetooth_headsets_with_PulseAudio
  # https://nixos.wiki/wiki/Bluetooth#Enabling_A2DP_Sink
  hardware = {
    bluetooth = {
      enable = true;
      settings = {
        General = {
          Enable = "Source,Sink,Media,Socket";
        };
      };
    };
    # opengl.enable = true;
    pulseaudio.enable = false;
  };

  fonts.fonts = with pkgs; [
    noto-fonts
    noto-fonts-extra
    open-dyslexic
  ];

  nix = {
    # https://github.com/NixOS/nix/issues/2208
    extraOptions = ''
      keep-outputs = true
      experimental-features = nix-command flakes
    '';

    package = pkgs.nixUnstable;

    trustedUsers = [ "root" "rgoulter" ];
  };

  nixpkgs.config.allowUnfree = true;

  programs.ssh.startAgent = true;
  programs.steam.enable = true;

  # https://nixos.wiki/wiki/PipeWire
  # rtkit is optional but recommended
  security.rtkit.enable = true;

  services = {
    # Enable the OpenSSH daemon.
    openssh.enable = true;

    # https://nixos.wiki/wiki/PipeWire
    # https://nixos.wiki/wiki/PipeWire#Bluetooth_Configuration
    pipewire  = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
      media-session.config.bluez-monitor.rules = [
        {
          # Matches all cards
          matches = [ { "device.name" = "~bluez_card.*"; } ];
          actions = {
            "update-props" = {
              "bluez5.reconnect-profiles" = [ "hfp_hf" "hsp_hs" "a2dp_sink" ];
              # mSBC: higher quality in calls.
              # mSBC is not expected to work on all headset + adapter combinations.
              "bluez5.msbc-support" = true;
              # SBC-XQ: higher audio
              # SBC-XQ is not expected to work on all headset + adapter combinations.
              "bluez5.sbc-xq-support" = true;
            };
          };
        }
        {
          matches = [
            # Matches all sources
            { "node.name" = "~bluez_input.*"; }
            # Matches all outputs
            { "node.name" = "~bluez_output.*"; }
          ];
          actions = {
            "node.pause-on-idle" = false;
          };
        }
      ];
    };

    udev = {
      # https://docs.qmk.fm/#/faq_build?id=linux-udev-rules
      extraRules = ''
# Atmel DFU
SUBSYSTEMS=="usb", ATTRS{idVendor}=="03eb", ATTRS{idProduct}=="2ff4", TAG+="uaccess"

# STM32 DFU
SUBSYSTEMS=="usb", ATTRS{idVendor}=="0483", ATTRS{idProduct}=="df11", TAG+="uaccess"

# STM32duino 1eaf:0003
SUBSYSTEMS=="usb", ATTRS{idVendor}=="1eaf", ATTRS{idProduct}=="0003", TAG+="uaccess"
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
