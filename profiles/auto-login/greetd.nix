{
  services = {
    greetd = {
      enable = true;
      settings = {
        default_session = { command = "Hyprland"; };
        initial_session = {
          user = "freeman";
          command = "Hyprland";
        };
      };
    };
  };
}
