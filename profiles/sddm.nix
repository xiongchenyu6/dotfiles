{
  pkgs,
  config,
  lib,
  ...
}:
{
  environment = {
    systemPackages = with pkgs; [
      (sddm-astronaut.override {
        themeConfig = {
          "General.ScreenWidth" = "1920";
          ScreenHeight = "1080";
          ScreenPadding = "";
          Font = "Open Sans";
          FontSize = "13";
          KeyboardSize = "0.4";
          RoundCorners = "20";
          Locale = "";
          HourFormat = "HH:mm";
          DateFormat = "dddd d MMMM";
          HeaderText = "";
        };
      })
      catppuccin-sddm-corners
    ];
  };
  services = {

    displayManager = {
      sessionPackages = [ pkgs.hyprland ];
      sddm = {
        enable = true;
        theme = "catppuccin-sddm-corners";
        #theme = "sddm-astronaut-theme";
        wayland.enable = true;
        autoNumlock = true;
      };
      defaultSession = "hyprland";
    };
  };
}
