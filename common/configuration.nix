# Edit this configuration file to define what should be installed on


# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, lib, symlinkJoin, domain, ... }:


{
  networking = {
    domain = "freeman.engineer";
  };

  environment = {
    systemPackages = with pkgs; [
      # self.packages."${system}".bttc
      dig
      git
      wireguard-tools
      traceroute
      python3
      inetutils
      killall
      tree
      tmux
      tcpdump
    ];
  };

  services = {
    openssh = { enable = true; };
  };
}
