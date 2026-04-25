{
  pkgs,
  config,
  lib,
  ...
}:
{
  environment = {
    systemPackages = with pkgs; [
      sddm-astronaut
      catppuccin-sddm-corners
    ];
  };

  services = {
    displayManager = {
      sessionPackages = [ pkgs.niri ];
      sddm = {
        enable = true;
        #theme = "catppuccin-sddm-corners";
        theme = "sddm-astronaut-theme";
        wayland.enable = true;
        autoNumlock = true;
      };
      defaultSession = "niri";
    };
  };
}
