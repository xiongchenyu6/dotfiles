{ lib, ... }: {
  imports = [ ./wayland.nix ];

  services = {
    greetd = {
      enable = true;
      settings = rec {
        initial_session = {
          user = "freeman.xiong";
          command = lib.mkDefault "Hyprland";
        };
        default_session = initial_session;
      };
    };
  };
}
