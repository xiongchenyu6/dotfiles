{ pkgs, ... }:
{
  services = {
    fwupd = {
      enable = true;
    };
    gpm.enable = true;

    # logind = {
    #   lidSwitch = "ignore";
    # };
    # Enable CUPS to print documents.
    gnome = {
      gnome-remote-desktop.enable = true;
      at-spi2-core.enable = true;
    };
    printing = {
      enable = true;
    };
    upower = {
      enable = true;
      ignoreLid = true;
    };
    pipewire = {
      #systemWide = true;
      enable = true;
      alsa = {
        enable = true;
        support32Bit = true;
      };
      jack = {
        enable = true;
      };
      pulse = {
        enable = true;
      };
    };

    # If you want to use JACK applications, uncomment this
    #jack.enable = true;
    udisks2 = {
      enable = true;
    };
    blueman = {
      enable = true;
    };

    udev = {
      packages = [ pkgs.yubikey-personalization ];
      extraRules = ''
        ACTION=="add", SUBSYSTEM=="backlight", KERNEL=="intel_backlight", MODE="0666", RUN+="${pkgs.coreutils}/bin/chmod a+w /sys/class/backlight/%k/brightness"
        ACTION=="add", SUBSYSTEMS=="usb", ATTRS{idVendor}=="0483", ATTRS{idProduct}=="3748", MODE="770", GROUP="wheel", SYMLINK+="stlinkv2-1_%n"
      '';
    };

    # use the example session manager (no others are packaged yet so this is enabled by default,
    # no need to redefine it in your config for now)
    #media-session.enable = true;
  };

  hardware = {
    bluetooth = {
      enable = true;
    };
    graphics = {
      enable = true;
      enable32Bit = true;
    };
    ledger.enable = true;
  };
}
