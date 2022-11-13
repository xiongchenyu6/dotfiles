{pkgs, ...}: {
  services = {
    # Enable CUPS to print documents.
    gnome = {
      gnome-remote-desktop.enable = true;
      at-spi2-core.enable = true;
    };
    printing = {enable = true;};
    upower = {enable = true;};
    pipewire = {
      enable = true;
      audio.enable = true;
      alsa = {
        enable = true;
        support32Bit = true;
      };
      jack = {
        enable = true;
      };
      pulse = {enable = true;};
    };

    # If you want to use JACK applications, uncomment this
    #jack.enable = true;
    udisks2 = {enable = true;};
    blueman = {enable = true;};

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
    pulseaudio = {enable = false;};
    bluetooth = {enable = true;};
    opengl.enable = true;
  };
  # Enable sound with pipewire.
  sound = {enable = true;};
}
