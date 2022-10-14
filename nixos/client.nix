{ config, pkgs, lib, symlinkJoin, ... }:
{
  imports =
    let
      ls = dir: builtins.map (f: (dir + "/${f}")) (builtins.attrNames (builtins.readDir dir));
    in
    [ ]
    ++ (ls ./common-apps)
    ++ (ls ./client-apps)
    ++ (ls ./common-components)
    ++ (ls ./client-components)
  ;

  nix = {
    generateNixPathFromInputs = true;
    generateRegistryFromInputs = true;
    linkInputs = true;
  };

  environment = {
    systemPackages = with pkgs; [
      asciinema
      awscli2
      agenix
      clang
      cmake
      conky
      cabal2nix
      cachix
      discord
      devshell.cli
      neofetch
      exa
      feh
      fd
      jp2a
      pass
      procs
      tealdeer
      socat
      rustscan
      virtualbox
      gitAndTools.gitflow
      git-crypt
      geoip
      gnumake
      gh
      gopls
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
      (python3.withPackages (ps: [
        my_cookies
        epc
        python310Packages.orjson
        python310Packages.python-lsp-server
      ]))
      pinentry
      linuxPackages.ply
      wakatime
      node2nix
      nodejs
      nodePackages."typescript-language-server"
      nodePackages."bash-language-server"
      nvfetcher
      nixopsUnstable
      openssl
      protobuf
      plantuml
      ripgrep
      rnix-lsp
      tomb
      tronbox
      unzip
      vlc
      scrot
      stow
      slack
      statix
      wineWowPackages.staging
      wireshark
      #wpa_supplicant_gui
      wakatime
      whatsapp-for-linux
      xclip
      qq
      wechat-uos
      xscreensaver
      zoom-us
    ];
    pathsToLink = [ "/share/zsh" ];
  };

  services =
    {
      dbus = { enable = true; };

      trilium-server = {
        enable = true;
        noAuthentication = true;
      };

      gnome = { gnome-keyring = { enable = true; }; };

      openldap = {
        enable = true;
      };
    };

  users = {
    users = {
      freeman = {
        packages = with pkgs; [ tdesktop ];
      };

    };
  };
  programs = {
    atop = {
      enable = true;
      netatop = { enable = true; };
      atopgpu = { enable = true; };
    };
    nm-applet = { enable = true; };
  };

  home-manager = {
    users = {
      freeman = {
        imports = [ ../home/gui ];
      };
    };
  };

  nix = {
    extraOptions = ''
      keep-outputs = true
      keep-derivations = true
    '';
  };



}
