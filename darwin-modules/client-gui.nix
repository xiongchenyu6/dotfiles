# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{ pkgs, ... }:
{
  fonts = {
    packages = with pkgs; [
      nerd-fonts.hack
      jetbrains-mono
      noto-fonts
      noto-fonts-cjk-sans
      noto-fonts-cjk-serif
      font-awesome
    ];
  };

  environment = {
    systemPackages = with pkgs; [
    ];
  };
  homebrew = {
    enable = true;
    brews = [
    ];
    casks = [
      "google-chrome"
    ];
    taps = [
      "oven-sh/bun"
    ];
    global = {
      autoUpdate = true;
      brewfile = true;
    };
    # mac app store
    # masApps = { WireGuard = 1451685025; };
    onActivation = {
      autoUpdate = true;
      cleanup = "zap";
      upgrade = true;
    };
  };

  services = {
    postgresql = {
      enable = true;
      package = pkgs.postgresql;
      enableTCPIP = true;
    };
    spacebar = {
      enable = false;
      package = pkgs.spacebar;
      config = {
        position = "top";
        height = 32;
        title = "on";
        spaces = "on";
        clock = "on";
        power = "on";
        padding_left = 20;
        padding_right = 20;
        spacing_left = 25;
        spacing_right = 15;
        icon_font = ''"Hack Nerd Font:Regular:12.0"'';
        background_color = "0xff202020";
        foreground_color = "0xffa8a8a8";
        power_icon_color = "0xffcd950c";
        battery_icon_color = "0xffd75f5f";
        dnd_icon_color = "0xffa8a8a8";
        clock_icon_color = "0xffa8a8a8";
        power_icon_strip = " ";
        space_icon = "•";
        space_icon_strip = "1 2 3 4 5 6 7 8 9 10";
        display_separator = "on";
        display_separator_icon = "";
        space_icon_color = "0xff458588";
        space_icon_color_secondary = "0xff78c4d4";
        space_icon_color_tertiary = "0xfffff9b0";
        clock_icon = "";
        dnd_icon = "";
        clock_format = ''"%d/%m/%y %R"'';
        right_shell = "on";
        right_shell_icon = "";
        right_shell_command = "whoami";
        display = "all";
        spaces_for_all_displays = "on";
        debug_output = "on";
      };
      extraConfig = ''echo "spacebar config loaded..."'';
    };

    yabai = {
      enable = false;
      enableScriptingAddition = false;
      config = {
        layout = "bsp";
        focus_follows_mouse = "autoraise";
        mouse_follows_focus = "off";
        window_placement = "second_child";
        window_opacity = "off";
        top_padding = 36;
        bottom_padding = 10;
        left_padding = 10;
        right_padding = 10;
        window_gap = 10;
      };
    };
  };
}
