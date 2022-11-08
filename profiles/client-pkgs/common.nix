# Edit this configuration file to define what should be installed on

# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, lib, ... }:

{
  nix = {
    generateNixPathFromInputs = true;
    generateRegistryFromInputs = true;
    linkInputs = true;
    extraOptions = ''
      keep-outputs = true
      keep-derivations = true
    '';
  };

  environment = {
    systemPackages = with pkgs; [
      asciinema
      awscli2
      azure-cli
      agenix
      apg
      bundix
      clang
      clang-tools
      cmake
      conky
      cabal2nix
      cachix
      discord
      deploy-rs
      neofetch
      exa
      feh
      fd
      jp2a
      jdt-language-server
      pass
      procs
      tealdeer
      tendermint
      socat
      rustscan
      virtualbox
      gitAndTools.gitflow
      git-crypt
      geoip
      gnumake
      gh
      gopls
      graphviz
      gcc
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
      (python3.withPackages (ps:
        with python3.pkgs; [
          my_cookies
          pkgs.epc
          orjson
          python-lsp-server
          cmake-language-server
        ]))
      pinentry
      patchelf
      linuxPackages.ply
      wakatime
      metals
      nixfmt
      nixpkgs-fmt
      node2nix
      nodejs
      nodePackages."bash-language-server"
      nodePackages."prettier"
      nodePackages."typescript-language-server"
      nvfetcher
      # nix-alien
      # nix-index-update
      nixopsUnstable
      openssl
      protobuf
      plantuml
      ripgrep
      rnix-lsp
      tdesktop
      terraform
      terranix
      tronbox
      mysql
      unzip
      vlc
      scrot
      stow
      slack
      statix
      sops
      scalafmt
      solc-select
      litecli
      wineWowPackages.staging
      wireshark
      winklink
      #wpa_supplicant_gui
      wakatime
      whatsapp-for-linux
      xclip
      qq
      wechat-uos
      xscreensaver
      yaml-language-server
      zoom-us
    ];
  };
}
