# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{
  fonts = {
    enableDefaultFonts = true;
    enableGhostscriptFonts = true;
    fontconfig = {
      enable = true;
      defaultFonts = {
        emoji = ["Noto Color Emoji"];
        monospace = [
          "Noto Sans Mono CJK SC"
          "DejaVu Sans Mono"
        ];
        sansSerif = [
          "Noto Sans CJK SC"
          "Source Han Sans SC"
        ];
        serif = [
          "Noto Serif CJK SC"
          "Source Han Serif SC"
        ];
      };
    };
  };

  imports = [./common.nix];

  networking = {domain = "freeman.engineer";};

  # Select internationalisation properties.
  i18n = {
    defaultLocale = "en_US.UTF-8";
    supportedLocales = ["zh_CN.UTF-8/UTF-8" "en_US.UTF-8/UTF-8"];
    # inputMethod = {
    #   enabled = "fcitx5";
    #   fcitx5.addons = with pkgs; [
    #     fcitx5-mozc
    #     fcitx5-gtk
    #     fcitx5-chinese-addons
    #   ];
    # };
  };

  nix = {
    daemonCPUSchedPolicy = "idle";
    daemonIOSchedClass = "idle";
    gc = {randomizedDelaySec = "1h";};
    optimise.automatic = true;
  };

  system = {
    stateVersion = "22.11";
  }; # Did you read the comment?
  programs = {
    zsh = {
      enableCompletion = true;
    };
  };
}
