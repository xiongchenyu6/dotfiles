{ ... }: {
  imports = [ ./wayland.nix ];
  services = { getty = { autologinUser = "freeman.xiong"; }; };
}
