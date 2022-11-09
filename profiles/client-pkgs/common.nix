# Edit this configuration file to define what should be installed on

# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ pkgs, ... }:

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
      cachix
      deploy-rs
      neofetch
      exa
      feh
      fd
      jdt-language-server
      tealdeer
      tendermint
      socat
      rustscan
      gitAndTools.gitflow
      git-crypt
      gh
      gopls
      graphviz
      (python3.withPackages (ps:
        with python3.pkgs; [
          six
          orjson
          python-lsp-server
          cmake-language-server
        ]))
      wakatime
      metals
      nixfmt
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
      plantuml
      ripgrep
      rnix-lsp
      terraform
      terranix
      tronbox
      unzip
      stow
      slack
      statix
      sops
      scalafmt
      solc-select
      litecli
      wireshark
      winklink
      #wpa_supplicant_gui
      wakatime
      yaml-language-server
      zoom-us
    ];
  };
}
