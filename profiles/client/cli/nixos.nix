# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{ pkgs, ... }:
{
  imports = [ ./common.nix ];

  environment = {
    systemPackages = with pkgs; [
      apg
      clang
      clang-tools
      cava
      cmake
      gcc
      gdb
      geoip
      gnumake
      grafana-loki
      gparted
      imagemagick
      inetutils
      ifuse
      jp2a
      lldb
      llvm
      lsof
      manix
      #my_cookies
      openfortivpn
      pass
      pciutils
      patchelf
      procs
      qemu_kvm
      tpm2-tools
      zssh
    ];

    pathsToLink = [ "/share/zsh" ];
  };
  services.pcscd.enable = true;

  programs = {
    npm = {
      enable = true;
    };
    nix-ld.enable = true;
  };
}
