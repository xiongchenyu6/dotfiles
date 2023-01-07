{ pkgs, ... }: {
  imports = [ ./wayland.nix ];

  services = {
    greetd = {
      enable = true;
      settings = rec {
        initial_session = {
          user = "freeman.xiong";
          command = "${pkgs.hyprland}/bin/Hyprland";
        };
        default_session = initial_session;
      };
    };
  };
}
