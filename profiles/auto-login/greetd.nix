{ pkgs, lib, ... }:
{
  environment = {
    etc = {
      "hyprland.conf" = {
        text = ''
          exec-once = ${pkgs.greetd.regreet}/bin/regreet; ${pkgs.hyprland}/bin/hyprctl dispatch exit
          misc {
              disable_hyprland_logo = true
              disable_splash_rendering = true
              disable_hyprland_qtutils_check = true
          }
        '';
        user = "greeter";
      };
    };
  };

  programs.regreet.enable = true;

  services = {
    greetd = {
      enable = true;
      settings = {
        initial_session = {
          user = "freeman.xiong";
          command = lib.mkDefault "Hyprland";
        };
        defualt_session = {
          user = "greeter";
          command = "${pkgs.hyprland}/bin/Hyprland --config /etc/hyprland.conf";
        };
      };
    };
  };
}
