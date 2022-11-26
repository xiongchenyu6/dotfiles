# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{
  pkgs,
  lib,
  config,
  ...
}: {
  imports = [./common.nix];

  environment = {
    systemPackages = with pkgs; [
      apg
      aspell
      aspellDicts.en
      bundix
      baidupcs-go
      clang
      clang-tools
      cmake
      cabal2nix
      cava
      geoip
      gnumake
      gimp
      haskell-language-server
      (python3.withPackages (_: with python3.pkgs; [my_cookies mutagen]))
      (haskellPackages.ghcWithPackages (_:
        with haskellPackages;
        with pkgs.haskell.lib; [
          apply-refact
          cabal-install
          hlint
          stylish-haskell
          hasktags
          hoogle
          pandoc
          hadolint
        ]))
      heroku
      imagemagick
      inetutils
      lsof
      llvm
      lldb
      vscode-extensions.vadimcn.vscode-lldb
      pinentry
      pass
      procs
      patchelf
      jp2a
      linuxPackages.ply
      tdesktop
      unrar-wrapper
      manix
      mpv
      nix-alien
      wineWowPackages.staging
      #wpa_supplicant_gui
      whatsapp-for-linux
      qq
      virtmanager
      wechat-uos
    ];

    pathsToLink = ["/share/zsh"];
  };

  services = {
    dbus = {enable = true;};

    gnome = {gnome-keyring = {enable = true;};};

    openldap = {enable = true;};
    ympd = {enable = true;};
  };

  programs = {
    npm = {enable = true;};
    atop = {
      enable = true;
      netatop = {enable = true;};
      atopgpu = {enable = true;};
    };
    nm-applet = {enable = true;};
    nix-ld.enable = true;
    wireshark = {enable = true;};
  };

  environment.variables = {
    NIX_LD_LIBRARY_PATH = lib.makeLibraryPath (config.systemd.packages ++ config.environment.systemPackages);
    NIX_LD = "${pkgs.glibc}/lib/ld-linux-x86-64.so.2";
  };
}
