# Edit this configuration file to define what should be installed on


# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, lib, symlinkJoin, domain, ... }:
{
  krb5 = {
    enable = true;
    realms = {
      "FREEMAN.ENGINEER" = {
        admin_server = "freeman.engineer";
        kdc = "freeman.engineer";
        default_domain = "freeman.engineer";
        kpasswd_server = "freeman.engineer";
      };
    };
    libdefaults = {
      default_realm = "FREEMAN.ENGINEER";
    };
    domain_realm = {
      "freeman.engineer" = "FREEMAN.ENGINEER";
      ".freeman.engineer" = "FREEMAN.ENGINEER";
    };
  };


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
