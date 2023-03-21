{ pkgs, ... }: {
  services = {
    xserver = {
      enable = true;
      layout = "us";
      displayManager = {
        lightdm = { enable = true; };
        autoLogin = {
          enable = true;
          user = "freeman.xiong";
        };

        session = [{
          manage = "desktop";
          name = "xsession";
          start = "exec $HOME/.xsession";
        }];
        defaultSession = "xsession";
      };
      # Configure keymap in X11
      xkbVariant = "dvp";
      xkbModel = "dvorak-programmer";

      xkbOptions = "caps:ctrl_modifier";
      autoRepeatDelay = 180;
      autoRepeatInterval = 60;
      # Enable touchpad support (enabled default in most desktopManager).
      libinput = { enable = true; };
      excludePackages = [ pkgs.xterm ];
      # Enable automatic login for the user.
    };
  };
  environment = { systemPackages = with pkgs; [ arandr conky scrot xclip ]; };
}
