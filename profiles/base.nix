# Edit this configuration file to define what should be installed on

# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, lib, ... }:

let
  init-kerberos-ticket = pkgs.writeText "init-kerberos.sh" ''
    pass=1
    host=`hostname -f`
    kadmin -p admin -w $pass -q "delprinc -force host/$host"
    kadmin -p admin -w $pass -q "ank -randkey host/$host"
    kadmin -p admin -w $pass -q "ktrem host/$host"
    kadmin -p admin -w $pass -q "ktadd host/$host"
    systemctl restart sssd
  '';
in {
  sops.defaultSopsFile = ../secrets/example.yaml;
  sops.secrets.example_key = { };
  # sops.secrets."example_array" = { };
  # sops.secrets."example_booleans[0]" = {
  #   format = "yaml";
  #   # can be also set per secret
  #   sopsFile = ../secrets/example.yaml;
  # };

  networking = {
    domain =
      builtins.trace config.sops.secrets."example_key".path "freeman.engineer";
  };

  time = { timeZone = "Asia/Singapore"; };

  # Select internationalisation properties.
  i18n = {
    defaultLocale = "en_US.UTF-8";
    supportedLocales = [ "zh_CN.UTF-8/UTF-8" "en_US.UTF-8/UTF-8" ];
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
      ssh-to-age
      cyrus_sasl
      mycli
    ];
  };
  system = { stateVersion = "22.11"; }; # Did you read the comment?
}

