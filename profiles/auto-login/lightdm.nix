{pkgs, ...}: {
  services = {
    xserver = {
      enable = true;
      layout = "us";
      displayManager = {
        lightdm = {enable = true;};
        autoLogin = {
          enable = true;
          user = "freeman";
        };
        session = [
          {
            manage = "desktop";
            name = "xsession";
            start = "exec $HOME/.xsession";
          }
        ];
        defaultSession = "xsession";
      };
      # Configure keymap in X11
      xkbOptions = "caps:ctrl_modifier";
      autoRepeatDelay = 180;
      autoRepeatInterval = 60;
      # Enable touchpad support (enabled default in most desktopManager).
      libinput = {enable = true;};
      # Enable automatic login for the user.
    };
  };
  environment = {
    systemPackages = with pkgs; [
      scrot
    ];
  };
}
