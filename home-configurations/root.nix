{ ezModules, osConfig, ... }:
{
  programs.ssh.enable = true;

  home = {
    username = "root";
    stateVersion = "26.05";
    homeDirectory = osConfig.users.users.root.home;
  };
}
