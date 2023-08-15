{ pkgs, config, lib, ... }: {
  imports = [
    ./kernel.nix
    ./network.nix
    ./security.nix
    ./ssh-harden.nix
    ./virtualisation.nix
  ];
}
