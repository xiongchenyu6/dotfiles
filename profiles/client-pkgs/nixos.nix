# Edit this configuration file to define what should be installed on

# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, lib, ... }:

{
  imports = [ ./common.nix ];

  environment = {
    systemPackages = with pkgs; [
      apg
      bundix
      conky
      clang
      clang-tools
      cmake
      cabal2nix
      discord
      geoip
      gnumake
      gh
      gopls
      graphviz
      haskell-language-server
      (haskellPackages.ghcWithPackages (self:
        with haskellPackages;
        with pkgs.haskell.lib; [
          apply-refact
          cabal-install
          hlint
          stylish-haskell
          hasktags
          hoogle
          pandoc
        ]))
      heroku
      imagemagick
      ispell
      lsof
      pinentry

      pass
      procs

      gcc
      jp2a
      patchelf
      linuxPackages.ply
      tdesktop
      vlc
      scrot
      wineWowPackages.staging
      #wpa_supplicant_gui
      whatsapp-for-linux
      xclip
      qq
      wechat-uos
    ];

    pathsToLink = [ "/share/zsh" ];
  };

  services = {
    dbus = { enable = true; };

    gnome = { gnome-keyring = { enable = true; }; };

    openldap = { enable = true; };
  };

  programs = {
    atop = {
      enable = true;
      netatop = { enable = true; };
      atopgpu = { enable = true; };
    };
    nm-applet = { enable = true; };
    nix-ld.enable = true;
    wireshark = { enable = true; };
  };

}
