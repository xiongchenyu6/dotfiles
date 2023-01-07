{ pkgs, ... }: {
  imports = [ ./wayland.nix ];

  services = {
    kmscon = {
      enable = true;
      hwRender = true;
      autologinUser = "freeman";
      fonts = [{
        name = "Hack Nerd Font";
        package = pkgs.nerdfonts.override { fonts = [ "Hack" ]; };
      }];
    };
  };
}
