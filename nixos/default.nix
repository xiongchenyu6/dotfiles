# Edit this configuration file to define what should be installed on

# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, lib, ... }:

{
  networking = { domain = "freeman.engineer"; };

  time = { timeZone = "Asia/Singapore"; };

  i18n = {
    defaultLocale = "en_US.UTF-8";
    supportedLocales = [ "zh_CN.UTF-8/UTF-8" "en_US.UTF-8/UTF-8" ];
    # Select internationalisation properties.
  };

  system.nixos = { tags = [ "test" "add-feat" ]; };

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
      vim
      tcpdump
      file
      schema2ldif
      cyrus_sasl
      mycli
    ];
  };
  system = { stateVersion = "22.11"; }; # Did you read the comment?

}

