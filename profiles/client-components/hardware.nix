{ config, pkgs, options, lib, ... }:

{
  services = {
    # Enable CUPS to print documents.
    printing = { enable = true; };
    upower = { enable = true; };
    pipewire = {
      enable = true;
      alsa = {
        enable = true;
        support32Bit = true;
      };
      pulse = { enable = true; };
    };

    # If you want to use JACK applications, uncomment this
    #jack.enable = true;
    udisks2 = { enable = true; };
    blueman = { enable = true; };

    udev = {
      extraRules = ''
        ACTION=="add", SUBSYSTEM=="backlight", KERNEL=="intel_backlight", MODE="0666", RUN+="${pkgs.coreutils}/bin/chmod a+w /sys/class/backlight/%k/brightness"
      '';
    };

    # use the example session manager (no others are packaged yet so this is enabled by default,
    # no need to redefine it in your config for now)
    #media-session.enable = true;

  };

  hardware = {
    pulseaudio = { enable = false; };
    bluetooth = { enable = true; };
  };
  # Enable sound with pipewire.
  sound = { enable = true; };

}
