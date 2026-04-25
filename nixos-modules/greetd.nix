{ pkgs, lib, ... }:
{
  services = {
    greetd = {
      enable = true;
      settings = {
        initial_session = {
          user = lib.mkDefault "freeman.xiong";
          command = lib.mkDefault "niri-session";
        };
        default_session = {
          user = "greeter";
          command = "${pkgs.greetd.tuigreet}/bin/tuigreet --time --remember --cmd niri-session";
        };
      };
    };
  };

  # Unlock gnome-keyring on login. gnome-keyring itself is enabled in
  # nixos-modules/gui.nix (services.gnome.gnome-keyring.enable).
  security.pam.services = {
    login.enableGnomeKeyring = true;
    greetd.enableGnomeKeyring = true;
    swaylock = { };
  };
}
