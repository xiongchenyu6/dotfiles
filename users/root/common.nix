# Edit this configuration file to define what should be installed on
{ pkgs, ... }: {
  users.users.root = { shell = pkgs.zsh; };
  programs.zsh.enable = true;
  home-manager.users = {
    root = { imports = [ ../profiles/use-remote-builder.nix ]; };
  };
}
