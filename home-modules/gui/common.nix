{ pkgs, lib, ... }:
{
  # Home Manager needs a bit of information about you and the
  # paths it should manage.

  home = {
    packages =
      with pkgs;
      [
        #appimage-run
        #discord
        cloc
        claude-monitor
        minicom
        doctl
        gdrive
        gtrash
        #freerdp
        #dmidecode
        # jetbrains.idea-ultimate
        # jetbrains.rider
        xournalpp
        slack
        zoom-us
        ueberzugpp
        #ytfzf
        usbutils
        zip
        #vault
        #solana-cli
        # expect mkpasswd conflict
        gpg-tui
        sysz
        ncdu
        lazygit
        lazydocker
        #(warp-terminal.override { waylandSupport = true; })
        #kmon
        termshark
        glow # markdown viewer
        lnav
        lego
        #gitbutler
        # zed-editor
        nixd
        #v4l-utils
        dotnetCorePackages.sdk_8_0
        #sui
        foundry
        #record_screen
        apg
        #cava # audio visualizer
        cmake
        gcc
        openfortivpn
        gnumake
        geoip
        github-copilot-cli
        manix
        grafana-loki
        imagemagick
        inetutils
        #ifuse
        lsof
        #my_cookies
        glib
        pass
        patchelf
        procs
        ansible.out
        #qemu_kvm
        #tpm2-tools
      ]
      ++ lib.optionals pkgs.stdenv.isLinux [
        google-chrome # Keep Chrome in Nix for Linux
        jp2a # Marked broken on Darwin
        lm_sensors # Linux-only hardware monitoring
        fwupd # Firmware update daemon (Linux-only)
        gparted # Disk partitioning GUI (Linux-only)
        pciutils # PCI utilities (mostly Linux-specific)
      ];
  };

  programs = {
    alacritty = {
      enable = true;
      settings = {
        # opacity = 0.9;
        font = {
          size = 10;
          normal = {
            family = "Hack Nerd Font";
          };
          bold = {
            family = "Hack Nerd Font";
          };
          italic = {
            family = "Hack Nerd Font";
          };
          bold_italic = {
            family = "Hack Nerd Font";
          };
        };
        cursor = {
          style = {
            shape = "Beam";
            blinking = "Always";
          };
        };
        keyboard = {
          bindings = [
            {
              key = "Space";
              mods = "Control|Shift";
              mode = "~Search";
              action = "ToggleViMode";
            }
            {
              key = "Return";
              mods = "Command|Shift";
              action = "SpawnNewInstance";
            }
          ];
        };
      };
    };

    noti = {
      enable = true;
    };

  };
}
